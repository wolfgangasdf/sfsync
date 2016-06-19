package sfsync.synchro

import sfsync.CF
import sfsync.Main.{MyWorker, myTask}
import sfsync.synchro.Actions._
import sfsync.store._
import sfsync.util.{Logging, StopWatch}

import scalafx.Includes._
import scala.collection.mutable.ListBuffer
import scala.language.{postfixOps, reflectiveCalls}

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
          } else {
            se.compareSetAction(newcache = false)
          }
        } else {
          se.compareSetAction(newcache = false)
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
    debug("TTT e took = " + swse.getTimeRestart)
    res
  }
}

class Profile(server: Server, protocol: Protocol, subfolder: SubFolder) extends Logging {
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null
  var syncLog = ""
  var UIUpdateInterval = 0.5
  var profileInitialized = false

  /* how to abort? set 3 vars to true:
  local.stopRequested and remote.stopRequested: running transfers are aborted and partial files deleted
  stopProfileRequested: check at end of each loop, throw ProfileAbortedException if so. take care of cleanup in finally{}!
   */
  @volatile var stopProfileRequested = false // TODO remove all this?

  class ProfileAbortedException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

  val taskIni = new myTask { override def call(): Unit = {
    updateTitle("Initialize connections...")
    Cache.loadCache(server.id.getValueSafe)

    if (protocol.executeBefore.value != "") {
      updateProgr(0, 100, "execute 'before'...")
      import sys.process._
      val res = protocol.executeBefore.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'before' command!")
      }
    }

    local = new LocalConnection(true, false) {
      remoteBasePath = server.localFolder.value
    }
    val uri = MyURI(protocol.protocoluri.value)
    debug(s"puri = ${protocol.protocoluri.value}  proto = ${uri.protocol}")
    updateProgr(50, 100, "initialize remote connection...")

    remote = uri.protocol match {
      case "sftp" => new SftpConnection(false, server.cantSetDate.value, uri)
      case "file" => new LocalConnection(false, server.cantSetDate.value)
      case _ => throw new RuntimeException("wrong protocol: " + uri.protocol)
    }
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder.getValueSafe
    profileInitialized = true
    updateProgr(100, 100, "done!")
  } }


  val taskCompFiles = new myTask { override def call(): Unit = {
    updateTitle("CompareFiles...")
    val sw = new StopWatch // for timing meas

    // reset table
    updateProgr(0, 100, "resetting database...")

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
        se.lSize = -1
        se.lTime = -1
        se.rSize = -1
        se.rTime = -1
        se.relevant = true
      } else {
        se.relevant = false
      }
    })
    debug("sw: resetting table: " + sw.getTimeRestart)

    updateProgr(50, 100, "find files loca and remote...")
    val swUIupdate = new StopWatch

    def acLocRem(vf: VirtualFile, isloc: Boolean, updact: (VirtualFile) => Unit): Unit = {
      //debug(s"found loc=$isloc : " + vf)
      if (swUIupdate.doit(UIUpdateInterval)) updact(vf)

      Cache.cache.merge(vf.path,
        new SyncEntry(A_UNCHECKED, if (isloc) vf.modTime else 0, if (isloc) vf.size else -1,
                                   if (!isloc) vf.modTime else 0, if (!isloc) vf.size else -1,
          0, 0, -1, vf.path.endsWith("/"), true),
        new java.util.function.BiFunction[SyncEntry, SyncEntry, SyncEntry] {
          override def apply(ov: SyncEntry, nv: SyncEntry): SyncEntry = {
            if (isloc) {
              ov.lTime = vf.modTime ; ov.lSize = vf.size
            } else {
              ov.rTime = vf.modTime ; ov.rSize = vf.size
            }
            ov
          }
        }
      )
    }
    val taskListLocal = new myTask {
      override def call(): Unit = {
        updateTitle("Find local files")
        subfolder.subfolders.foreach(local.list(_, server.filterRegexp.getValueSafe, vf => acLocRem(vf, isloc = true, vf => updateMessage(s"found ${vf.path}")), recursive = true))
      }
    }
    val taskListRemote = new myTask {
      override def call(): Unit = {
        updateTitle("Find remote files")
        subfolder.subfolders.foreach(remote.list(_, server.filterRegexp.getValueSafe, vf => acLocRem(vf, isloc = false, vf => updateMessage(s"found ${vf.path}")), recursive = true))
        debug("tlr: end")
      }
    }
    taskListLocal.onCancelled = () => { debug(" local cancelled!") }
    taskListRemote.onCancelled = () => { debug(" rem cancelled!") }
    taskListLocal.onSucceeded = () => { debug(" local succ!") }
    taskListRemote.onSucceeded = () => { debug(" rem succ!") }
    taskListLocal.onFailed = () => { debug(" local failed!") }
    taskListRemote.onFailed = () => { debug(" rem failed!") }
    MyWorker.runTask(taskListLocal)
    MyWorker.runTask(taskListRemote)

    // TODO: this does not work with i8160white.... very strange

    while (!(taskListLocal.isDone && taskListRemote.isDone)) { // ignore exceptions / errors!
      Thread.sleep(100)
    }
//    Helpers.runUI { debug("tll: " + taskListLocal.getState) }
//    Helpers.runUI { debug("tlr: " + taskListRemote.getState) }
    debug("sw: finding files: " + sw.getTimeRestart)

    // compare entries
    updateProgr(76, 100, "comparing...")
    sw.restart()
    info("*********************** compare sync entries")
    val haveChanges = CompareStuff.compareSyncEntries()
    debug("havechanges1: " + haveChanges)

    debug("sw: comparing: " + sw.getTimeRestart)
    set(haveChanges)
    updateProgr(100, 100, "done")
  } }

  val taskSynchronize = new myTask { override def call(): Unit = {
    updateTitle("Synchronize")
    updateProgr(0, 100, "startup...")

    val sw = new StopWatch
    val swUIupdate = new StopWatch
    var iii = 0
    Cache.cache.iterate((it, path, se) => if (se.relevant) iii += 1)
    val tosync = iii
    iii = 0

    def dosync(path: String, se: SyncEntry) = {
      iii += 1

      var showit = false
      val relevantSize = if (se.action == A_USELOCAL) se.lSize else if (se.action == A_USEREMOTE) se.rSize else 0
      if (relevantSize > 10000) showit = true

      var msg = ""
      remote.onProgress = (progressVal: Double) => {
        val pv = (100 * progressVal).toInt
        updateMessage(s" [${CF.amap(se.action)}]: $path...$pv%")
      }

      if (showit || swUIupdate.doit(UIUpdateInterval)) {
        msg = s" [${CF.amap(se.action)}]: $path..."
        updateProgr(iii.toDouble, tosync, msg)
      }

//      try {
        se.action match {
          case A_MERGE => throw new Exception("merge not implemented yet!")
          case A_RMLOCAL | A_RMBOTH => local.deletefile(path, se.lTime); se.delete = true; se.relevant = false
          case A_RMREMOTE | A_RMBOTH => remote.deletefile(path, se.rTime); se.delete = true; se.relevant = false
          case A_USELOCAL => val nrt = remote.putfile(path, se.lTime)
            se.rTime = nrt
            se.rSize = se.lSize
            se.cSize = se.lSize
            se.lcTime = se.lTime
            se.rcTime = nrt
            se.relevant = false
          case A_USEREMOTE => remote.getfile(path, se.rTime)
            se.lTime = se.rTime
            se.lSize = se.rSize
            se.cSize = se.rSize
            se.rcTime = se.rTime
            se.lcTime = se.rTime
            se.relevant = false
          case A_ISEQUAL => se.cSize = se.rSize
            se.lcTime = se.lTime
            se.rcTime = se.rTime
            se.relevant = false
          case A_SKIP =>
          case A_CACHEONLY => se.delete = true
          case aa => throw new UnsupportedOperationException("unknown action: " + aa)
        }
//      } catch {
//        case ir: InterruptedException => thr
//        case e: Exception =>
//          error("sync exception:", e)
//          se.action = A_SYNCERROR
//          se.delete = false
//          syncLog += (e + "[" + path + "]" + "\n")
//      }
//      if (stopProfileRequested) throw new ProfileAbortedException("stopreq")
    }

//    try {
      for (state <- List(1, 2)) {
        // delete and add dirs must be done in reverse order!
        debug("syncing state = " + state)
        state match {
          case 1 => // delete
            Cache.cache.iterate((it, path, se) => {
              if (se.relevant && List(A_RMBOTH, A_RMLOCAL, A_RMREMOTE).contains(se.action)) {
                dosync(path, se)
              }
            }, reversed = true)
          case _ => // put/get and others
            Cache.cache.iterate((it, path, se) => {
              if (se.relevant) dosync(path, se)
            })
        }
        // update cache: remove removed/cacheonly files
        Cache.cache.iterate((it, path, se) => {
          if (se.delete) it.remove()
        })
      } // for state
//    } catch {
//      case abex: ProfileAbortedException =>
//        debug("ProfileAbortedException!!! " + abex)
//      case ex: Exception => error("Unexpected other exception:" + ex)
//    } finally {
//      sw.stopPrintTime("TTTTTTTTT synchronized in ")
//
//      set(syncLog)
//      if (syncLog != "") {
//        throw new Exception("Errors during synchronization\n(mind that excluded files are not shown):\n" + syncLog)
//      }
//
//    }
  } }

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

//  // initiate stopping of profile actions and cleanup.
//  def abortProfile() {
//    debug("abortProfile!")
//    local.stopRequested = true
//    remote.stopRequested = true
//    stopProfileRequested = true
//    // TODO F: make sure that stopped, then save cache! also call cleanup here?
//    Cache.saveCache(server.id.getValueSafe)
//  }
//
  // cleanup (transfers must be stopped before)
  val taskCleanup = new myTask { override def call(): Unit = {
    updateTitle("Cleanup profile...")
    updateProgr(1, 100, "Save cache...")
    Cache.saveCache(server.id.getValueSafe)
    updateProgr(50, 100, "Cleanup...")
    if (remote != null) remote.cleanUp()
    if (local != null) local.cleanUp()
    remote = null
    local = null

    updateProgr(75, 100, "Execute after command...")
    if (protocol.executeAfter.value != "") {
      import sys.process._
      val res = protocol.executeAfter.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'after' command!")
      }
    }
    updateProgr(100, 100, "done!")
  } }



}

