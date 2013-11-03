package sfsync.synchro

/*
this runs not in ui thread, so use runUI and runUIwait!
for 20000 local files (testlocalmany):
rev 99227ee (before new compare algo):
reset: 1s, then 0.8s
timings: 16s, 13s (view all) or 12s (only changes)
 */

import scala.collection.mutable.ListBuffer
import Actions._
import sfsync.store._
import sfsync.{Main, FilesView}
import sfsync.Helpers._
import sfsync.util.{Logging, StopWatch}
import akka.actor.ActorDSL._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{reflectiveCalls, postfixOps}
import scala.concurrent.ExecutionContext.Implicits.global
import org.squeryl.Session
import org.squeryl.PrimitiveTypeMode._
import sfsync.Main.Dialog
import akka.actor.ActorRef

//import sfsync.synchro.addFile

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
    //println("**** all relevant:") ; q.map(se => println(se))
    // iterate over all folders not in cache and check if it has parent folder that has been synced before in current set
    val qfsnocache = q.where(se => se.path.like("%/") and se.cSize === -1)
    //println("**** all dirs without cache:") ; qfsnocache.map(se => println(se))
    MySchema.files.update(qfsnocache.map(se => {
      //println("check if there is synced parent in current set of: " + se.path)
      var tmpf = se.path
      var haveCacheParentDir = false
      var doit = true
      while (!haveCacheParentDir && doit) {
        tmpf = getParentFolder(tmpf)
        if (tmpf != "/") {
          //println(s"  checking parent $tmpf")
          val pq = q.where(se => se.path === tmpf)
          if (pq.size == 1) {
            if (pq.head.cSize != -1) haveCacheParentDir = true
          } else {
            doit = false // parent not in relevant set in DB
          }
        } else doit = false
      }
      // compare...
      se.hasCachedParent = haveCacheParentDir
      se.iniAction2(!haveCacheParentDir)
      //println(s"  havecachedparent: $haveCacheParentDir => action: " + CF.amap(se.action))
      se
    }))
    debug("TTT a took = " + swse.getTimeRestart)
    // iterate over all folders that are cacheed
    val qfscache = q.where(se => se.path.like("%/") and se.cSize <> -1)
    //println("**** all dirs with cache:") ; qfscache.map(se => println(se))
    MySchema.files.update(qfscache.map(se => {
      se.hasCachedParent = true
      se.iniAction2(newcache = false)
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
          se.iniAction2(newcache = true) // test
          //print("  [c]")
        } else {
          se.iniAction2(newcache = false)
          //print("  [b]")
        }
      } else {
        se.iniAction2(newcache = false)
        //print("  [a]")
      }
      //println(" ==> " + CF.amap(se.action))
      se
    }))
    debug("TTT c took = " + swse.getTimeRestart)

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

  @volatile var threadLocal: Thread = null
  @volatile var threadRemote: Thread = null
  var receiveActor: ActorRef = null

  def init() {
    view.updateSyncButton(allow = false)
    if (protocol.executeBefore.value != "") {
      runUIwait {
        Main.Status.status.value = "execute 'before'..."
      }
      import sys.process._
      val res = protocol.executeBefore.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'before' command!")
      }
    }

    runUIwait { Main.Status.status.value = "ready" }

    local = new LocalConnection(true) {
      remoteBasePath = server.localFolder.value
    }
    debug("puri = " + protocol.protocoluri.value)
    val uri = MyURI(protocol.protocoluri.value)
    debug("proto = " + uri.protocol)
    runUIwait { Main.Status.status.value = "ini remote connection..." }
    remote = uri.protocol match {
      case "sftp" => new SftpConnection(false,uri)
      case "file" => new LocalConnection(false)
      case _ => { throw new RuntimeException("wrong protocol: " + uri.protocol) }
    }
    runUIwait { Main.Status.status.value = "ready" }
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder
  }


  def compare() {
    debug("compare() in thread " + Thread.currentThread().getId)
    // reset
    runUIwait { view.enableActions = true }
    // reset table
    runUIwait { Main.Status.status.value = "Resetting database..." }
    debug("resetting table...")
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
    var receiveSession: Session = null
    // the receive actor
    var lfiles = 0
    var rfiles = 0
    val swUI = new StopWatch // for UI update
    receiveActor = actor(Main.system, name = "receive")(new Act {
      var finished = 2*subfolder.subfolders.length // cowntdown for lists
      debug("receiveList in thread " + Thread.currentThread().getId)
      become {
        case addFile(vf, islocal) => {
//          debug("  received " + vf)
          if (swUI.getTime > UIUpdateInterval) {
            runUIwait { // give UI time
              Main.Status.status.value = "Find files... " + vf.path
              Main.Status.local.value = lfiles.toString
              Main.Status.remote.value = rfiles.toString
              view.updateSyncEntries()
            }
            swUI.restart()
          }
          if (islocal) lfiles += 1 else rfiles += 1
          if (receiveSession==null) {
            receiveSession = CacheDB.getSession
            debug("created receivesession " + receiveSession + " in Thread " + Thread.currentThread().getId)
          }
          using (receiveSession) {
            val q = MySchema.files.where(se => se.path === vf.path)
            if (q.size == 0) { // new entry
              val senew = new SyncEntry(vf.path, A_UNCHECKED, if (islocal) vf.modTime else 0, if (islocal) vf.size else -1,
                if (!islocal) vf.modTime else 0, if (!islocal) vf.size else -1,0,-1,true)
              MySchema.files.insert(senew)
              //debug("added new " + senew)
            } else {
              val se = q.single
              if (islocal) { se.lTime = vf.modTime ; se.lSize = vf.size }
              else         { se.rTime = vf.modTime ; se.rSize = vf.size }
              // optimization if entry not modified and in cache: not really needed... 3 secs gain for 20k files
//              if (se.lSize != -1 && se.rSize != -1 && se.cSize != -1) se = se.iniAction2(false)
              MySchema.files.update(se)
              //debug("updated   " + se)
            }
          }
        }
        case 'done => {
          finished -= 1
          if (finished == 0) {
            debug("receiveList: remotelistfinished!")
          }
        }
        case 'replyWhenDone => if (finished==0) {
          sender ! 'done
          debug("exit actor receiveList")
          context.stop(self)
        } else sender ! 'notyet
      }
    })

    info("*********************** start find files local and remote...")
    runUIwait { Main.Status.status.value = "Find files..." }
    future {
      threadLocal = Thread.currentThread()
      subfolder.subfolders.map(local.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
    }
    future {
      threadRemote = Thread.currentThread()
      subfolder.subfolders.map(remote.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
    }
    implicit val timeout = Timeout(36500 days)
    info("*********************** wait until all received...")
    while (Await.result(receiveActor ? 'replyWhenDone, Duration.Inf) != 'done) { Thread.sleep(100) }

    info("*********************** list finished")
    runUIwait {
      Main.Status.local.value = lfiles.toString
      Main.Status.remote.value = rfiles.toString
      Main.Status.status.value = "Compare files..."
    }
    // init with best guess
    info("*********************** compare sync entries")
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
    debug("  comparing etc took " + sw.getTimeRestart)
    if (runSync == true) synchronize()
  }

  def synchronize() {
    debug("synchronize() in thread " + Thread.currentThread().getId)
    runUIwait { Main.Status.status.value = "Synchronize..." }
    val sw = new StopWatch
    val swd = new StopWatch
    var iii = 0
    val syncSession = CacheDB.getSession
    using(syncSession) {
      for (state <- List(1,2)) { // delete and add dirs must be done in reverse order!
        debug("syncing state = " + state)
        val q = state match {
          case 1 => { // delete
          from (MySchema.files) (se => where((se.relevant === true) and (se.action in List(A_RMBOTH,A_RMLOCAL,A_RMREMOTE)))
            select se
            orderBy(se.path desc) // this allows deletion of dirs!
          )}
          case _ => { // put/get and others
            from (MySchema.files) (se => where(se.relevant === true)
              select se
              orderBy(se.path asc) // this allows deletion of dirs!
            )}
        }
        MySchema.files.update(q.map(se => {
          iii += 1

          var showit = false
          if (se.action == A_USELOCAL) { if (se.lSize>10000) showit = true }
          if (se.action == A_USEREMOTE) { if (se.rSize>10000) showit = true }
          if (swd.getTime > UIUpdateInterval || showit) {
            syncSession.connection.commit() // TODO it does not seem to work
            runUIwait { // give UI time
              Main.Status.status.value = "Synchronize(" + iii + "): " + se.path
              view.updateSyncEntries()
            }
            swd.restart()
          }
          try {
            // debug("syncing  " + se)
            se.action match {
              case A_MERGE => throw new Exception("merge not implemented yet!")
              case A_RMLOCAL|A_RMBOTH => { local.deletefile(se.path,se.lTime) ; se.delete = true; se.relevant = false }
              case A_RMREMOTE|A_RMBOTH => { remote.deletefile(se.path, se.rTime) ; se.delete = true; se.relevant = false }
              case A_USELOCAL => { remote.putfile(se.path, se.lTime) ; se.rTime=se.lTime; se.rSize=se.lTime; se.cSize = se.lSize; se.cTime = se.lTime; se.relevant = false }
              case A_USEREMOTE => { remote.getfile(se.path, se.rTime) ; se.lTime=se.rTime; se.lSize=se.rTime; se.cSize = se.rSize; se.cTime = se.rTime; se.relevant = false }
              case A_ISEQUAL => { se.cSize = se.rSize; se.cTime = se.rTime; se.relevant = false }
              case A_SKIP => { se.relevant = false }
              case A_CACHEONLY => { se.delete = true }
              case _ => { }
            }
          } catch {
            case e: Exception => {
              error("sync exception:", e)
              se.action = A_SYNCERROR
              syncLog += (e + "\n")
            }
          }
          se
        })) // update loop
        MySchema.files.deleteWhere(se => se.delete === true)
      } // for state
      // update cache: remove removed files
    } // using(syncsession)
    syncSession.close

    sw.printTime("TTTTTTTTT synchronized in ")
    var switchBackToSettings = true
    runUIwait {
      if (syncLog != "") {
        switchBackToSettings = false
        Dialog.showMessage("Errors during synchronization\n(mind that excluded files are not shown):\n" + syncLog)
      }
      view.updateSyncEntries()
      view.enableActions = false
      Main.Status.status.value = "Finished synchronize"
      Main.Status.local.value = ""
      Main.Status.remote.value = ""
    }
    stop()
  }
  def stop(switchBackToSettings: Boolean = true) {
    debug("stopping profile...")
    if (remote != null) remote.finish()
    if (local != null) local.finish()
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
      debug("profile stopped!")
      Main.doCleanup()
    }
  }



}

