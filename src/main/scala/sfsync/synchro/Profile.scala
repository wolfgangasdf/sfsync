package sfsync.synchro

import sfsync.CF
import sfsync.util.Helpers.{MyWorker, myTask}
import sfsync.synchro.Actions._
import sfsync.store._
import sfsync.util.{Helpers, Logging, StopWatch}
import sfsync.util.Helpers._

import scalafx.Includes._
import scala.collection.mutable.ListBuffer
import scala.language.{postfixOps, reflectiveCalls}

import javafx.{concurrent => jfxc}

class TransferProtocol (
  var uri: String,
  var basefolder: String
)

class Profile(server: Server, protocol: Protocol, subfolder: SubFolder) extends Logging {
  var cache: ListBuffer[VirtualFile] = _
  var local: GeneralConnection = _
  var remote: GeneralConnection = _
  val UIUpdateInterval = 0.5
  var profileInitialized = false

  class ProfileAbortedException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

  val taskIni: myTask = new myTask { override def call(): Unit = {
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

    local = new LocalConnection(protocol, true) {
      remoteBasePath = server.localFolder.value
    }
    val uri = MyURI(protocol.protocoluri.value)
    debug(s"puri = ${protocol.protocoluri.value}  proto = ${uri.protocol}")
    updateProgr(50, 100, "initialize remote connection...")

    remote = uri.protocol match {
      case "sftp" => new SftpConnection(protocol, false, uri)
      case "file" => new LocalConnection(protocol, false)
      case _ => throw new RuntimeException("wrong protocol: " + uri.protocol)
    }
    if (Helpers.failat == 1) throw new UnsupportedOperationException("fail 1")
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder.getValueSafe
    profileInitialized = true
    updateProgr(100, 100, "done!")
  } }


  val taskCompFiles: myTask = new myTask { override def call(): Unit = {
    updateTitle("CompareFiles...")
    val sw = new StopWatch // for timing meas

    // reset table
    updateProgr(0, 100, "resetting database...")

    var cacheall = false
    for (sf <- subfolder.subfolders) if (sf == "") cacheall = true
    // remove cache orphans (happens if user doesn't click synchronize
    Cache.cache.iterate((it, _, se) => if (se.cSize == -1) it.remove())
    // ini files
    Cache.cache.iterate((_, path, se) => {
      var addit = cacheall
      if (!cacheall) for (sf <- subfolder.subfolders) if (path.startsWith("/" + sf + "/")) addit = true
      if (addit) {
        se.action = A_UNCHECKED; se.lSize = -1; se.lTime = -1; se.rSize = -1; se.rTime = -1; se.relevant = true
      } else {
        se.relevant = false
      }
    })
    debug("sw: resetting table: " + sw.getTimeRestart)

    updateProgr(50, 100, "Find files local and remote...")
    val swUIupdate = new StopWatch

    def acLocRem(vf: VirtualFile, isloc: Boolean, updact: (VirtualFile) => Unit): Unit = {
      //debug(s"found loc=$isloc : " + vf)
      if (swUIupdate.doit(UIUpdateInterval)) updact(vf)

      Cache.cache.merge(vf.path,
        new SyncEntry(A_UNCHECKED, if (isloc) vf.modTime else 0, if (isloc) vf.size else -1,
                                   if (!isloc) vf.modTime else 0, if (!isloc) vf.size else -1,
          0, 0, -1, vf.path.endsWith("/"), true),
        (ov: SyncEntry, _: SyncEntry) => {
          if (isloc) {
            ov.lTime = vf.modTime
            ov.lSize = vf.size
          } else {
            ov.rTime = vf.modTime
            ov.rSize = vf.size
          }
          ov
        }
      )
    }
    val taskListLocal = new myTask { override def call(): Unit = {
      updateTitle("Find local files")
      subfolder.subfolders.foreach(local.list(_, server.filterRegexp.getValueSafe, vf => acLocRem(vf, isloc = true, vf => updateMessage(s"found ${vf.path}")), recursive = true))
    } }
    val taskListRemote = new myTask { override def call(): Unit = {
      updateTitle("Find remote files")
      subfolder.subfolders.foreach(remote.list(_, server.filterRegexp.getValueSafe, vf => acLocRem(vf, isloc = false, vf => updateMessage(s"found ${vf.path}")), recursive = true))
    } }

    taskListLocal.onCancelled = () => { debug(" local cancelled!") }
    taskListRemote.onCancelled = () => { debug(" rem cancelled!") }
    taskListLocal.onSucceeded = () => { debug(" local succ!") }
    taskListRemote.onSucceeded = () => { debug(" rem succ!") }
    taskListLocal.onFailed = () => { error(" local failed!") }
    taskListRemote.onFailed = () => { debug(" rem failed!") }

    MyWorker.runTask(taskListLocal)
    MyWorker.runTask(taskListRemote)

    while (!(taskListLocal.isDone && taskListRemote.isDone)) { // ignore exceptions / errors!
      Thread.sleep(100)
    }

    debug("sw: finding files: " + sw.getTimeRestart)

    val res = runUIwait {
      debug("state after list: " + taskListLocal.getState + "  remote:" + taskListRemote.getState)
      if (taskListLocal.getState == jfxc.Worker.State.FAILED) taskListLocal.getException
      else if (taskListRemote.getState == jfxc.Worker.State.FAILED) taskListRemote.getException
      else if (taskListLocal.getState == jfxc.Worker.State.CANCELLED) new InterruptedException("Cancelled local task")
      else if (taskListRemote.getState == jfxc.Worker.State.CANCELLED) new InterruptedException("Cancelled remote task")
      else null
    }
    if (res != null) throw res.asInstanceOf[Exception]

    // compare entries
    updateProgr(76, 100, "comparing...")
    sw.restart()
    info("*********************** compare sync entries")
    val haveChanges = Comparison.compareSyncEntries()
    debug("havechanges1: " + haveChanges)

    debug("sw: comparing: " + sw.getTimeRestart)
    set(haveChanges)
    updateProgr(100, 100, "done")
  } }

  val taskSynchronize: myTask = new myTask { override def call(): Unit = {
    info("*********************** synchronize")
    updateTitle("Synchronize")
    updateProgr(0, 100, "startup...")

    var syncLog = ""
    val swUIupdate = new StopWatch
    var totalTransferSize = 0.0
    var transferredSize = 0.0
    Cache.cache.iterate((_, _, se) => if (se.relevant) {
      se.action match {
        case A_USELOCAL => totalTransferSize += se.lSize
        case A_USEREMOTE => totalTransferSize += se.rSize
        case _ =>
      }
    })
    var ignoreErrors = false

    def dosync(path: String, se: SyncEntry): Unit = {
      var showit = false
      val relevantSize = if (se.action == A_USELOCAL) se.lSize else if (se.action == A_USEREMOTE) se.rSize else 0
      if (relevantSize > 10000) showit = true

      var msg = ""
      remote.onProgress = (progressVal: Double, bytesPerSecond: Double) => {
        val pv = (100 * progressVal).toInt
        updateMessage(s" [${CF.amap(se.action)}]: $path...$pv% (${Helpers.tokMGTPE(bytesPerSecond)}B/s)")
      }

      if (showit || swUIupdate.doit(UIUpdateInterval)) {
        msg = s" [${CF.amap(se.action)}]: $path..."
        updateProgr(transferredSize, totalTransferSize, msg)
      }

      try {
        if (Helpers.failat == 5) throw new UnsupportedOperationException("fail 5")
        se.action match {
          case A_MERGE => throw new UnsupportedOperationException("Merge not implemented yet!")
          case A_RMLOCAL => local.deletefile(path, se.lTime); se.delete = true; se.relevant = false
          case A_RMREMOTE => remote.deletefile(path, se.rTime); se.delete = true; se.relevant = false
          case A_RMBOTH => local.deletefile(path, se.lTime); remote.deletefile(path, se.rTime); se.delete = true; se.relevant = false
          case A_USELOCAL => val nrt = remote.putfile(path, se.lTime)
            se.rTime = nrt; se.rSize = se.lSize; se.cSize = se.lSize; se.lcTime = se.lTime; se.rcTime = nrt; se.relevant = false
            transferredSize += se.lSize
          case A_USEREMOTE => remote.getfile(path, se.rTime)
            se.lTime = se.rTime; se.lSize = se.rSize; se.cSize = se.rSize; se.rcTime = se.rTime; se.lcTime = se.rTime; se.relevant = false
            transferredSize += se.rSize
          case A_ISEQUAL => se.cSize = se.rSize; se.lcTime = se.lTime; se.rcTime = se.rTime; se.relevant = false
          case A_SKIP =>
          case A_CACHEONLY => se.delete = true
          case aa => throw new UnsupportedOperationException("unknown action: " + aa)
        }
      } catch {
        case e: InterruptedException => throw e
        case e: Exception =>
          // TODO on first sync exception, ask user if to stop, continue, or continue ignoring errors
          error("sync exception:", e)
          se.action = A_SYNCERROR
          se.delete = false
          syncLog += (e + "[" + path + "]" + "\n")
          updateMessage("Failed: " + path + ": " + e)
          if (!ignoreErrors) {
            if (runUIwait(dialogOkCancel("Error", "Synchronization Error. Press OK to continue ignoring errors, Cancel to abort.", s"File: $path:\n${e.getMessage}")) == true)
              ignoreErrors = true
            else
              throw new Exception("Exception(s) during synchronize:\n" + syncLog)
          }
          // many exceptions are very slow, problem is stacktrace: http://stackoverflow.com/a/569118. Impossible to disable ST via Runtime.getRuntime()
          Thread.sleep(600) // to keep sfsync responsive...
      }
    }

    for (state <- List(1, 2)) {
      // delete and add dirs must be done in reverse order!
      debug("syncing state = " + state)
      state match {
        case 1 => // delete
          Cache.cache.iterate((_, path, se) => {
            if (local.interrupted.get || remote.interrupted.get) throw new InterruptedException("profile: connections interrupted")
            if (se.relevant && List(A_RMBOTH, A_RMLOCAL, A_RMREMOTE).contains(se.action)) {
              dosync(path, se)
            }
          }, reversed = true)
        case _ => // put/get and others
          Cache.cache.iterate((_, path, se) => {
            if (local.interrupted.get || remote.interrupted.get) throw new InterruptedException("profile: connections interrupted")
            if (se.relevant) dosync(path, se)
          })
      }
      // update cache: remove removed/cacheonly files
      Cache.cache.iterate((it, _, se) => {
        if (se.delete) it.remove()
      })
    }
    if (syncLog != "") throw new Exception("Exception(s) during synchronize:\n" + syncLog)
  } }

  // init action to make local as remote
  def iniLocalAsRemote(): Unit = {
    Cache.cache.iterate( (_, _, se) => {
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
    Cache.cache.iterate( (_, _, se) => {
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

  // cleanup (transfers must be stopped before)
  val taskCleanup: myTask = new myTask { override def call(): Unit = {
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

