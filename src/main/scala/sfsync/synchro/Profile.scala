package sfsync.synchro

/*
this runs not in ui thread, so use runUI and runUIwait!
 */

import sfsync.synchro.Actions._
import sfsync.store._
import sfsync.Main.Dialog
import sfsync.{Main, FilesView}
import sfsync.Helpers._
import sfsync.util.{Logging, StopWatch}

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{reflectiveCalls, postfixOps}
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorDSL._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

import org.squeryl.Session
import org.squeryl.PrimitiveTypeMode._

class TransferProtocol (
  var uri: String,
  var basefolder: String
)

object Actions {
  val A_UNCHECKED = -99
  val A_UNKNOWN = -1
  val A_ISEQUAL = 0
  val A_USELOCAL = 1
  val A_USEREMOTE = 2
  val A_MERGE = 3
  val A_RMLOCAL = 4
  val A_RMREMOTE = 5
  val A_CACHEONLY = 6
  val A_RMBOTH = 7
  val A_SYNCERROR = 8
  val A_SKIP = 9
  val ALLACTIONS = List(-99,-1,0,1,2,3,4,5,6,7,8,9)
}

case class addFile(vf: VirtualFile, islocal: Boolean)

object CompareStuff extends Logging {
  def getParentFolder(path: String) = {
    path.split("/").dropRight(1).mkString("/") + "/"
  }
  // compare, update database entries only.
  def compareSyncEntries() = {
    val swse = new StopWatch
    // q contains all entries to be checked
    val q = MySchema.files.where(se => (se.relevant === true) and (se.action === A_UNCHECKED))
    // update hasCachedParent for all folders not in cache and check if it has parent folder that has been synced before in current set
    val qfsnocache = q.where(se => se.path.like("%/") and se.cSize === -1)
    MySchema.files.update(qfsnocache.map(se => {
      var tmpf = se.path
      var haveCachedParentDir = false
      var doit = true
      while (!haveCachedParentDir && doit) {
        tmpf = getParentFolder(tmpf)
        if (tmpf != "/") {
          val pq = q.where(se => se.path === tmpf)
          if (pq.size == 1) {
            if (pq.head.cSize != -1) haveCachedParentDir = true
          } else {
            doit = false // parent not in relevant set in DB
          }
        } else doit = false
      }
      se.hasCachedParent = haveCachedParentDir
      se.compareSetAction(newcache = !haveCachedParentDir) // compare
      se
    }))
    debug("TTT a took = " + swse.getTimeRestart)
    // iterate over all folders that are cacheed
    val qfscache = q.where(se => se.path.like("%/") and se.cSize <> -1)
    //println("**** all dirs with cache:") ; qfscache.map(se => println(se))
    MySchema.files.update(qfscache.map(se => {
      se.hasCachedParent = true
      se.compareSetAction(newcache = false)
      se
    }))
    debug("TTT b took = " + swse.getTimeRestart)
    // iterate over the rest: all files.
    //println("**** checking all files:")
    //        val qfiles = q.minus(qfsnocache).minus(qfscache) // not implemented :-(
    val qfiles = q.where(se => not (se.path.like("%/")) )
    MySchema.files.update(qfiles.map(se => {
      //println("   file: " + se.path)
      if (se.cSize == -1) { // only get parent folder for unknown files, faster!
        val parent = getParentFolder(se.path)
        if (!MySchema.files.where(sex => sex.path === parent).head.hasCachedParent) {
          se.compareSetAction(newcache = true) // test
          //print("  [c]")
        } else {
          se.compareSetAction(newcache = false)
          //print("  [b]")
        }
      } else {
        se.compareSetAction(newcache = false)
        //print("  [a]")
      }
      //println(" ==> " + CF.amap(se.action))
      se
    }))
    debug("TTT c took = " + swse.getTimeRestart)

    // iterate over all folders that will be deleted: check that other side is not modified below
    val delfolds = MySchema.files.where(se => (se.relevant === true) and
      se.path.like("%/") and ( se.action === A_RMLOCAL or se.action === A_RMREMOTE ) )
    delfolds.foreach(sef => {
      // all folders below that have different action than folder sef
      val subthings = MySchema.files.where(se => (se.relevant === true) and se.path.like(sef.path + "%") and se.action <> sef.action)
      if (subthings.size > 0) { // fishy, mark all children and parent '?'
        sef.action = A_UNKNOWN
        val subthings2 = MySchema.files.where(se => (se.relevant === true) and se.path.like(sef.path + "%"))
        MySchema.files.update(subthings2.map(se => {
          se.action = A_UNKNOWN
          se
        }))
      }
    })

    // return true if changes
    !MySchema.files.where(se => (se.relevant === true) and (se.action <> A_ISEQUAL)).isEmpty
  }

}
class Profile  (view: FilesView, server: Server, protocol: Protocol, subfolder: SubFolder) extends Logging {
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null
  var syncLog = ""
  var UIUpdateInterval = 2.5
  var profileInitialized = false

  /* how to abort? set 3 vars to true:
  local.stopRequested and remote.stopRequested: running transfers are aborted and partial files deleted
  stopProfileRequested: check at end of each loop, throw ProfileAbortedException if so. take care of cleanup in finally{}!
   */
  @volatile var stopProfileRequested = false

  var receiveActor: ActorRef = null

  class ProfileAbortedException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

  def init() {
    view.updateSyncButton(allow = false)
    val progress = runUIwait { new Main.Progress("Initialize connection") {
      onAbortClicked = () => {
        abortProfile()
      }
    }}.asInstanceOf[Main.Progress]
    if (protocol.executeBefore.value != "") {
      runUIwait { progress.update(1.0, "execute 'before'...") }
      import sys.process._
      val res = protocol.executeBefore.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'before' command!")
      }
    }


    local = new LocalConnection(true) {
      remoteBasePath = server.localFolder.value
    }
    val uri = MyURI(protocol.protocoluri.value)
    debug(s"puri = ${protocol.protocoluri.value}  proto = ${uri.protocol}")
    runUIwait { progress.update(1.0, "initialize remote connection...") }
    remote = uri.protocol match {
      case "sftp" => new SftpConnection(false,uri)
      case "file" => new LocalConnection(false)
      case _ => throw new RuntimeException("wrong protocol: " + uri.protocol)
    }
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder
    profileInitialized = true
    runUIwait { progress.close() }
  }


  def compare() {
    debug("compare() in thread " + Thread.currentThread().getId)

    val progress = runUIwait { new Main.Progress("Compare files") {
      onAbortClicked = () => {
        abortProfile()
      }
    }}.asInstanceOf[Main.Progress]

    runUIwait { progress.update(1.0, "searching for files...") }
    // reset
    runUIwait { view.enableActions = true }
    // reset table
    runUIwait { progress.update(1.0, "resetting database...") }
    val sw = new StopWatch // for timing meas
    import org.squeryl.PrimitiveTypeMode.transaction
    import org.squeryl.PrimitiveTypeMode._
    var cacheall = false
    for (sf <- subfolder.subfolders) if (sf == "") cacheall = true
    transaction {
      // remove cache orphans (happens if user doesn't click synchronize
      MySchema.files.deleteWhere(se => se.cSize === -1)
      // ini files
      val q = from(MySchema.files)(s=>select(s))
      MySchema.files.update(q.map(a =>{
        var tmp = cacheall
        if (!cacheall) for (sf <- subfolder.subfolders) if (a.path.startsWith("/" + sf + "/")) tmp = true
        if (tmp) {
          a.action = A_UNCHECKED
          a.lSize = -1; a.lTime = -1
          a.rSize = -1; a.rTime = -1
          a.relevant = true
        } else {
          a.relevant = false
        }
        a
      }))
    }
    debug("  resetting table took " + sw.getTimeRestart)
    runUIwait { view.updateSyncEntries() }
    // the receive actor
    var lfiles = 0
    var rfiles = 0
    val swUI = new StopWatch // for UI update

    receiveActor = actor(Main.system, name = "receive")(new Act {
      var receiveActorSession: Session = null
      var finished = 2*subfolder.subfolders.length // cowntdown for lists
      debug("receiveList in thread " + Thread.currentThread().getId)
      become {
        case addFile(vf, islocal) =>
          //          debug("  received " + vf)
          if (swUI.getTime > UIUpdateInterval) {
            runUIwait { // give UI time
              progress.update(0.0, s"Find files... parsing:\n${vf.path}")
              Main.Status.local.value = lfiles.toString
              Main.Status.remote.value = rfiles.toString
              view.updateSyncEntries()
            }
            swUI.restart()
          }
          if (islocal) lfiles += 1 else rfiles += 1
          if (receiveActorSession==null) {
            receiveActorSession = CacheDB.getSession
            debug("created receivesession " + receiveActorSession + " in Thread " + Thread.currentThread().getId)
          }
          using (receiveActorSession) {
            val q = MySchema.files.where(se => se.path === vf.path)
            if (q.size == 0) { // new entry
              val senew = new SyncEntry(vf.path, A_UNCHECKED, if (islocal) vf.modTime else 0, if (islocal) vf.size else -1,
                if (!islocal) vf.modTime else 0, if (!islocal) vf.size else -1,0,-1,true)
              MySchema.files.insert(senew)
            } else { // update db entry
              val se = q.single
              if (islocal) { se.lTime = vf.modTime ; se.lSize = vf.size }
              else         { se.rTime = vf.modTime ; se.rSize = vf.size }
              MySchema.files.update(se)
            }
          }
        case 'done =>
          finished -= 1
          if (finished == 0) {
            debug("receiveList: remotelistfinished!")
          }
        case 'replyWhenDone => if (finished==0) {
          sender ! 'done
          debug("exit actor receiveList")
          context.stop(self)
        } else sender ! 'notyet
      }
    })

    info("*********************** start find files local and remote...")
    runUIwait { progress.update(1.0, "find files...") }
    Future {
      subfolder.subfolders.map(local.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
    }
    Future {
      subfolder.subfolders.map(remote.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
    }
    implicit val timeout = Timeout(36500 days)
    info("*********************** wait until all received...")
    var waitMore = true
    while (waitMore) {
      if (stopProfileRequested) waitMore = false else {
        debug("waiting...")
        try {
          implicit val timeout = Timeout(5 seconds)
          val future = receiveActor ? 'replyWhenDone // have to talk to actor in future!
          val result = Await.result(future, timeout.duration) // don't use Await.result directly on actor...
          waitMore = result != 'done
        } catch {
          case e: IllegalArgumentException => warn("ignoring exception " + e.getMessage)
        }
      }
      Thread.sleep(100)
    }
    debug("FINISHED waiting! waitmore = " + waitMore)

    if (stopProfileRequested) {
      debug("stopProfileRequested !! cleanup...")
      runUIwait {
        progress.close()
        Main.Status.status.value = "Compare files interrupted!"
        Main.Status.local.value = ""
        Main.Status.remote.value = ""
      }
      cleanupProfile(switchBackToSettings = true)
      return
    }

    info("*********************** list finished")
    runUIwait {
      Main.Status.local.value = lfiles.toString
      Main.Status.remote.value = rfiles.toString
      progress.update(1.0, "comparing files...")
    }
    // compare entries
    sw.restart()
    info("*********************** compare sync entries")
    val receiveSession = CacheDB.getSession
    debug("created receivesession " + receiveSession + " in Thread " + Thread.currentThread().getId)
    val haveChanges = using(receiveSession) {
      CompareStuff.compareSyncEntries()
    }
    if (receiveSession!=null) receiveSession.close

    info("*********************** compare: finish up")
    val runSync = runUIwait {
      Main.btCompare.setDisable(false)
      view.updateSyncEntries()
      val canSync = view.updateSyncButton(allow = true)
      if (haveChanges) {
        Main.Status.status.value = "Finished compare"
      } else {
        Main.Status.status.value = "Finished compare, no changes found. Synchronizing..."
      }
      !haveChanges && canSync
    }
    debug("  comparing etc took " + sw.getTime)
    runUIwait { progress.close() }

    if (runSync == true) synchronize() // if no changes found, run synchronize from here to update cache db!
  }

  def synchronize() {
    debug("synchronize() in thread " + Thread.currentThread().getId)
    runUIwait { Main.Status.clear() }

    val progress = runUIwait { new Main.Progress("Synchronize") {
      onAbortClicked = () => {
        abortProfile()
      }
    }}.asInstanceOf[Main.Progress]
    remote.onProgress = (progressVal: Double) => {
      runUIwait( {
        progress.updateProgressBar2(progressVal)
      })
    }

    val sw = new StopWatch
    val swUIupdate = new StopWatch
    val syncSession = CacheDB.getSession

    try {
      using(syncSession) {
        for (state <- List(1, 2)) {
          // delete and add dirs must be done in reverse order!
          var iii = 0
          debug("syncing state = " + state)
          val q = state match {
            case 1 => // delete
              from(MySchema.files)(se => where((se.relevant === true) and (se.action in List(A_RMBOTH, A_RMLOCAL, A_RMREMOTE)))
                select se
                orderBy (se.path desc) // this allows deletion of dirs!
              )
            case _ => // put/get and others
              from(MySchema.files)(se => where(se.relevant === true)
                select se
                orderBy (se.path asc) // this allows deletion of dirs!
              )
          }
          val tosync = q.size
          MySchema.files.update(q.map(se => {
            iii += 1

            var showit = false
            val relevantSize = if (se.action == A_USELOCAL) se.lSize else if (se.action == A_USEREMOTE) se.rSize else 0
            if (relevantSize > 10000) showit = true

            if (swUIupdate.getTime > UIUpdateInterval || showit) {
              // syncSession.connection.commit() // I cannot sync in update...
              // as long as squeryl doesn't know that I am the only write-session! how to do this?
              // view.updateSyncEntries() // this doesn't get the updates db...
              runUIwait {
                // update status
                progress.update(iii.toDouble/tosync, s"Synchronize($iii/$tosync):\n  Path: ${se.path}\n  Size: " + relevantSize)
              }
              swUIupdate.restart()
            } // update status
            try {
              se.action match {
                case A_MERGE => throw new Exception("merge not implemented yet!")
                case A_RMLOCAL | A_RMBOTH => local.deletefile(se.path, se.lTime); se.delete = true; se.relevant = false
                case A_RMREMOTE | A_RMBOTH => remote.deletefile(se.path, se.rTime); se.delete = true; se.relevant = false
                case A_USELOCAL => remote.putfile(se.path, se.lTime); se.rTime = se.lTime; se.rSize = se.lTime; se.cSize = se.lSize; se.cTime = se.lTime; se.relevant = false
                case A_USEREMOTE => remote.getfile(se.path, se.rTime); se.lTime = se.rTime; se.lSize = se.rTime; se.cSize = se.rSize; se.cTime = se.rTime; se.relevant = false
                case A_ISEQUAL => se.cSize = se.rSize; se.cTime = se.rTime; se.relevant = false
                case A_SKIP =>
                case A_CACHEONLY => se.delete = true
                case aa => throw new UnsupportedOperationException("unknown action: " + aa)
              }
            } catch {
              case e: Exception =>
                error("sync exception:", e)
                se.action = A_SYNCERROR
                se.delete = false
                syncLog += (e + "[" + se.path + "]" + "\n")
            }
            if (stopProfileRequested) throw new ProfileAbortedException("stopreq")
            se
          })) // update loop
          // update cache: remove removed/cacheonly files
          MySchema.files.deleteWhere(se => se.delete === true)
        } // for state
      }// using(syncsession)
    } catch {
      case abex: ProfileAbortedException  =>
        debug("ProfileAbortedException!!! " + abex)
      case ex: Exception => error("Unexpected other exception:" + ex)
    } finally {
      syncSession.close
      sw.stopPrintTime("TTTTTTTTT synchronized in ")
      var switchBackToSettings = true
      runUIwait {
        progress.close()
        var sbar = "Finished synchronize"
        if (syncLog != "") {
          switchBackToSettings = false
          Dialog.showMessage("Errors during synchronization\n(mind that excluded files are not shown):\n" + syncLog)
          sbar = "Synchronization error"
        }
        view.updateSyncEntries()
        view.enableActions = false
        Main.Status.clear()
      }
      cleanupProfile(switchBackToSettings)
    }
  }


  // initiate stopping of profile actions and cleanup.
  def abortProfile() {
    debug("abortProfile!")
    local.stopRequested = true
    remote.stopRequested = true
    stopProfileRequested = true
  }

  // cleanup (transfers must be stopped before)
  def cleanupProfile(switchBackToSettings: Boolean = true) {
    debug("cleanup profile...")
    if (remote != null) remote.cleanUp()
    if (local != null) local.cleanUp()
    remote = null
    local = null
    if (receiveActor != null) Main.system.stop(receiveActor)

    // execute after protocol
    if (protocol.executeAfter.value != "") {
      import sys.process._
      val res = protocol.executeAfter.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'after' command!")
      }
    }
    runUI {
      if (switchBackToSettings) Main.tabpane.selectionModel.get().select(0)
      Main.doCleanup()
    }
    debug("profile stopped!")
  }



}

