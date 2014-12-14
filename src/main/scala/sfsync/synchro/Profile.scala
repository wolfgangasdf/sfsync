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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{reflectiveCalls, postfixOps}
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorDSL._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.ActorRef

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._


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
  def compareSyncEntries(): Boolean = {
    val swse = new StopWatch
    // q contains all entries to be checked

    // to avoid first full sync: mark all folders that are subfolders of already synced folder
    // update hasCachedParent for all folders not in cache and check if it has parent folder that has been synced before in current set
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir) {
        var tmpf = path
        var haveCachedParentDir = false
        var doit = true
        while (!haveCachedParentDir && doit) {
          tmpf = getParentFolder(tmpf)
          if (tmpf != "/") {
            if (Cache.cache.containsKey(tmpf))
              if (Cache.cache.get(tmpf).cSize != -1) haveCachedParentDir = true
          } else {
            doit = false
          }
        }
        se.hasCachedParent = haveCachedParentDir
        se.compareSetAction(newcache = !haveCachedParentDir) // compare
      }
    })
    debug("TTT a took = " + swse.getTimeRestart)

    // iterate over all folders that are cacheed
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir && se.cSize != -1) {
        se.hasCachedParent = true
        se.compareSetAction(newcache = false) // compare
      }
    })
    debug("TTT b took = " + swse.getTimeRestart)

    // iterate over the rest: all files.
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && !se.isDir) {
        if (se.cSize == -1) { // only get parent folder for unknown files, faster!
        val parent = getParentFolder(path)
          if (!Cache.cache.get(parent).hasCachedParent) {
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
      }
    })
    debug("TTT c took = " + swse.getTimeRestart)

    // iterate over all folders that will be deleted: check that other side is not modified below
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir && List(A_RMLOCAL, A_RMREMOTE).contains(se.action)) {
        var fishy = false
        Cache.cache.iterate((it2, path2, se2) => {
          if (se.relevant && path2.startsWith(path) && se2.action != se.action) {
            fishy = true
          }
        })
        if (fishy) {
          Cache.cache.iterate((it2, path2, se2) => {
            if (se.relevant && path2.startsWith(path)) {
              se2.action = A_UNKNOWN
            }
          })
        }
      }
    })
    debug("TTT d took = " + swse.getTimeRestart)

    // return true if changes
    var res = false
    Cache.cache.iterate((it, path, se) => if (se.relevant && se.action != A_ISEQUAL) res = true)
    res
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

    var times: mutable.LinkedHashMap[String, Double] = mutable.LinkedHashMap()

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
    var cacheall = false
    for (sf <- subfolder.subfolders) if (sf == "") cacheall = true
    // remove cache orphans (happens if user doesn't click synchronize
    Cache.cache.iterate((it, path, se) => if (se.cSize == -1) it.remove())
    // ini files
    Cache.cache.iterate((it, path, se) => {
      var addit = cacheall
      if (!cacheall) for (sf <- subfolder.subfolders) if (path.startsWith("/" + sf + "/")) addit = true
      if (addit) {
        se.action = A_UNCHECKED
        se.lSize = -1; se.lTime = -1
        se.rSize = -1; se.rTime = -1
        se.relevant = true
      } else {
        se.relevant = false
      }
    })
    times += "resetting table" -> sw.getTimeRestart
    runUIwait { view.updateSyncEntries() }
    // the receive actor
    var lfiles = 0
    var rfiles = 0
    val swUI = new StopWatch // for UI update
    val hourOffset = server.hourOffset.value.toInt

    receiveActor = actor(Main.system, name = "receive")(new Act {
      var finished = 2*subfolder.subfolders.length // cowntdown for lists
      debug("receiveList in thread " + Thread.currentThread().getId)
      become {
        case addFile(vf, islocal) =>
          //debug("  received " + vf)
          if (swUI.getTime > UIUpdateInterval) {
            runUIwait { // give UI time
              progress.update(0.0, s"Find files... parsing:\n${vf.path}")
              Main.Status.local.value = lfiles.toString
              Main.Status.remote.value = rfiles.toString
              // TODO does not work of course... view.updateSyncEntries()
            }
            swUI.restart()
          }
          if (islocal) lfiles += 1 else rfiles += 1
          val se = Cache.cache.getOrDefault(vf.path, new SyncEntry(A_UNCHECKED, 0, -1, 0, -1, 0, -1, vf.path.endsWith("/"),true))
          if (islocal) { se.lTime = vf.modTime ; se.lSize = vf.size }
          else         { se.rTime = vf.modTime + hourOffset*60*60*1000 ; se.rSize = vf.size }
          Cache.cache += (vf.path -> se)
        case 'done =>
          finished -= 1
          debug("receivelist: new finished = " + finished)
          if (finished == 0) {
            debug("receiveList: remotelistfinished!")
          }
        case 'replyWhenDone => if (finished==0) {
          sender ! 'done
          debug("receiveList: replywhendone: done!")
          // context.stop(self) // must not do here
        } else sender ! 'notyet
      }
    })

    info("*********************** start find files local and remote...")
    runUIwait { progress.update(1.0, "find files...") }
    Future {
      subfolder.subfolders.map(local.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
      times += "future local list at end" -> sw.getTime
    }
    Future {
      subfolder.subfolders.map(remote.list(_, server.filterRegexp, receiveActor, recursive = true, viaActor = true))
      times += "future remote list at end sw=" -> sw.getTime
    }
    times += "before waiting" -> sw.getTime
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
          case e: java.util.concurrent.TimeoutException => warn("ignoring exception " + e.getMessage)
        }
      }
      Thread.sleep(250)
    }
    times += "finished waiting" -> sw.getTime
    debug("FINISHED waiting! time up to now=" + sw.getTime + "   waitmore = " + waitMore)

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
      view.updateSyncEntries()
      progress.update(1.0, "comparing files...")
    }
    // compare entries
    sw.restart()
    info("*********************** compare sync entries")
    val haveChanges = CompareStuff.compareSyncEntries()

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
    times += "comparing etc" -> sw.getTime
    runUIwait { progress.close() }

    debug("----------------- times:\n" + times.mkString("\n"))
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
    var iii = 0
    Cache.cache.iterate( (it, path, se) => { if (se.relevant) iii += 1 })
    val tosync = iii
    iii = 0

    def dosync(path: String, se: SyncEntry) = {
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
          progress.update(iii.toDouble/tosync, s"Synchronize($iii/$tosync):\n  Path: $path\n  Size: " + relevantSize)
        }
        swUIupdate.restart()
      } // update status
      try {
        se.action match {
          case A_MERGE => throw new Exception("merge not implemented yet!")
          case A_RMLOCAL | A_RMBOTH => local.deletefile(path, se.lTime); se.delete = true; se.relevant = false
          case A_RMREMOTE | A_RMBOTH => remote.deletefile(path, se.rTime); se.delete = true; se.relevant = false
          case A_USELOCAL => remote.putfile(path, se.lTime); se.rTime = se.lTime; se.rSize = se.lTime; se.cSize = se.lSize; se.cTime = se.lTime; se.relevant = false
          case A_USEREMOTE => remote.getfile(path, se.rTime); se.lTime = se.rTime; se.lSize = se.rTime; se.cSize = se.rSize; se.cTime = se.rTime; se.relevant = false
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
          syncLog += (e + "[" + path + "]" + "\n")
      }
      if (stopProfileRequested) throw new ProfileAbortedException("stopreq")
    }

    try {
      for (state <- List(1, 2)) {
        // delete and add dirs must be done in reverse order!
        debug("syncing state = " + state)
        val q = state match {
          case 1 => // delete
            Cache.cache.iterate( (it, path, se) => {
              if (se.relevant && List(A_RMBOTH, A_RMLOCAL, A_RMREMOTE).contains(se.action)) {
                dosync(path, se)
              }
            }, reversed = true)
          case _ => // put/get and others
            Cache.cache.iterate( (it, path, se) => {
              if (se.relevant) dosync(path, se)
            })
        }
        // update cache: remove removed/cacheonly files
        Cache.cache.iterate( (it, path, se) => { if (se.delete) it.remove() })
      } // for state
    } catch {
      case abex: ProfileAbortedException  =>
        debug("ProfileAbortedException!!! " + abex)
      case ex: Exception => error("Unexpected other exception:" + ex)
    } finally {
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

  // init action to make local as remote
  def iniLocalAsRemote(): Unit = {
    Cache.cache.iterate( (it, path, se) => {
      if (se.relevant && se.action != A_ISEQUAL) {
        se.action = se.action match {
          case A_RMREMOTE => A_USEREMOTE
          case A_USELOCAL => if (se.rSize > -1) A_USEREMOTE else A_RMLOCAL
          case A_UNKNOWN => if (se.rSize > -1) A_USEREMOTE else A_RMLOCAL
          case x => x
        }
      }
    })
  }

  // init action to make local as remote
  def iniRemoteAsLocal(): Unit = {
    Cache.cache.iterate( (it, path, se) => {
      if (se.relevant && se.action != A_ISEQUAL) {
        se.action = se.action match {
          case A_RMLOCAL => A_USELOCAL
          case A_USEREMOTE => if (se.lSize > -1) A_USELOCAL else A_RMREMOTE
          case A_UNKNOWN => if (se.lSize > -1) A_USELOCAL else A_RMREMOTE
          case x => x
        }
      }
    })
  }

  // initiate stopping of profile actions and cleanup.
  def abortProfile() {
    debug("abortProfile!")
    local.stopRequested = true
    remote.stopRequested = true
    stopProfileRequested = true
    // TODO: make sure that stopped, then save cache!
    Cache.saveCache(server.id)
  }

  // cleanup (transfers must be stopped before)
  def cleanupProfile(switchBackToSettings: Boolean = true) {
    debug("cleanup profile...")
    Cache.saveCache(server.id)
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

