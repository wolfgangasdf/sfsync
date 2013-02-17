package sfsync.synchro

/*
this runs not in ui thread, so use runUI and runUIwait!
 */

import scala.collection.mutable.ListBuffer
import Actions._
import akka.actor.ActorDSL._
import sfsync.store._
import sfsync.{Main, FilesView}
import sfsync.Helpers._
import util.StopWatch
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{reflectiveCalls, postfixOps}

class TransferProtocol (
  var uri: String,
  var basefolder: String
)

object Actions {
  val A_UNKNOWN = -1
  val A_NOTHING = 0
  val A_USELOCAL = 1
  val A_USEREMOTE = 2
  val A_MERGE = 3
  val A_RMLOCAL = 4
  val A_RMREMOTE = 5
  val A_CACHEONLY = 6
  val A_RMBOTH = 7
}
case object ComparedFile
class ComparedFile(val flocal: VirtualFile, val fremote: VirtualFile, val fcache: VirtualFile, val newcache: Boolean = false) {
  var action: Int = -9
  def isSynced = if (flocal != null) flocal == fremote else false

  override def toString: String = "A=" + action + " local:" + flocal + " remote:" + fremote + " cache:" + fcache
  override def hashCode = action.hashCode() + (if (flocal!=null) flocal.hashCode else 0)
    + (if (fremote!=null) fremote.hashCode else 0) + (if (fcache!=null) fcache.hashCode else 0)
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[ComparedFile] && (this.hashCode() == that.asInstanceOf[ComparedFile].hashCode())
  }

  // init with best guess
  if (flocal == null && fremote == null && fcache != null) { // cache only?
    action = A_CACHEONLY
  } else  if (flocal == fremote) { // just equal?
    action = A_NOTHING
  } else if (fcache == null) { // not in remote cache
    if (newcache) action = A_UNKNOWN // not equal and not in cache. unknown!
    else {
      if (flocal != null && fremote == null) action = A_USELOCAL // new local (cache not new)
      else if (flocal == null && fremote != null) action = A_USEREMOTE // new remote (cache not new)
      else action = A_UNKNOWN // not in cache but both present
    }
  } else { // in cache, newcache impossible
    if (flocal == fcache && fremote == null) action = A_RMLOCAL // remote was deleted (local still in cache)
    else if (flocal == null && fremote == fcache) action = A_RMREMOTE // local was deleted (remote still in cache)
    // both exist, as does fcache
    else if (flocal == fcache && fremote.modTime>flocal.modTime) action = A_USEREMOTE // flocal unchanged, remote newer
    else if (fremote == fcache && flocal.modTime>fremote.modTime) action = A_USELOCAL // fremote unchanged, local newer
    else action = A_UNKNOWN // both changed and all other strange things that might occur
  }
//  println("CF: " + toString)
  assert(action != -9)
}

case class CompareFinished()
case class RemoveCF(cf: ComparedFile)

class Profile  (view: FilesView, server: Server, protocol: Protocol, subfolder: SubFolder) {
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  var cache: ListBuffer[VirtualFile] = null
  var cacherelevant = new ListBuffer[VirtualFile] // only below subdir
  var local: GeneralConnection = null
  var remote: GeneralConnection = null
  var newcache: Boolean = false

  var remotecnt = 0

  def init() {
    runUIwait { Main.Status.status.value = "load cached files..." }
    cache = Cache.loadCache(server.id)
    if (cache.length == 0) {
      println("new cache!")
      newcache = true
      if (!subfolder.subfolders.contains("")) throw new scala.Exception("Cache empty: sync all files on first run!")
    }
    var cacheall = false
    cacherelevant = new ListBuffer[VirtualFile]
    subfolder.subfolders.foreach(x => {if (x == "") cacheall = true})
    if (subfolder.subfolders.isEmpty) cacheall = true
    if (!cacheall) {
      subfolder.subfolders.foreach(x => {cacherelevant ++= cache.filter(cf => cf.path.startsWith("/" + x + "/"))})
//      cacherelevant = cache.filter(cf => cf.path.startsWith(subfolder.subfolders.collect( { case s: String => "/" + s + "/"})))
    } else
      cacherelevant = cache
    if (protocol.executeBefore.value != "") {
      runUIwait { Main.Status.status.value = "execute 'before'..." }
      import sys.process._
      val res = protocol.executeBefore.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'before' command!")
      }
    }

    runUIwait { Main.Status.status.value = "ready" }

    local = new LocalConnection {
      remoteBasePath = server.localFolder.value
    }
    println("puri = " + protocol.protocoluri.value)
    val uri = MyURI(protocol.protocoluri.value)
    println("proto = " + uri.protocol)
    runUIwait { Main.Status.status.value = "ini remote connection..." }
    remote = uri.protocol match {
      case "sftp" => new SftpConnection(uri)
      case "file" => new LocalConnection
      case _ => { throw new RuntimeException("wrong protocol: " + uri.protocol) }
    }
    runUIwait { Main.Status.status.value = "ready" }
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder
  }

  def compare() {
    runUIwait { Main.Status.status.value = "list local files..." }
    println("***********************list local")

    var locall = new ListBuffer[VirtualFile]()
    StopWatch.timed("loaded local list in ") {
      for (sf <- subfolder.subfolders) locall ++= local.listrec(sf, server.filterRegexp, null)
    }
    runUIwait { Main.Status.local.value = locall.length.toString }
    runUIwait { Main.Status.status.value = "list remote files..." }
    println("***********************receive remote list")

    val sw = new StopWatch
    val receiveList = actor(Main.system)(new Act {
      var finished = subfolder.subfolders.length
      become {
        case rf: VirtualFile => {
          remotecnt += 1
          if (sw.getTime > 0.1) {
            runUIwait { // give UI time
              Main.Status.remote.value = remotecnt.toString
              Main.Status.status.value = "list " + rf.path
            }
            sw.restart()
          }
          val cachef = cacherelevant.find(x => x.path == rf.path).getOrElse(null)
          if (cachef != null) cachef.tagged = true // mark
          val localf = locall.find(x => x.path == rf.path).getOrElse(null)
          locall -= localf
          val cfnew = new ComparedFile(localf, rf, cachef, newcache)
          if (server.skipEqualFiles.value && rf == localf) {
            // files equal, just make sure cache is up to date!
            rf.tagged = true // so it does not appear as 'cacheonly'
            if (cachef == null) Cache.addupdate(rf) // only store non-existing cache files
          } else { // send to view
            comparedfiles += cfnew
            view.act ! cfnew // send it to view!
          }
        }
        case 'done => {
          finished -= 1
          if (finished == 0) {
            println("receiveList: remotelistfinished!")
            runUI {
              Main.Status.remote.value = remotecnt.toString
            }
          }
        }
        case 'replyWhenDone => if (finished==0) {
          sender ! 'done
          println("exit actor receiveList")
          context.stop(self)
        }
      }
    })
    val sw1 = new StopWatch
    subfolder.subfolders.map( sf => remote.listrec(sf, server.filterRegexp, receiveList) )
    sw1.printLapTime("TTTTTTT listrec alone needed ")
    implicit val timeout = Timeout(36500 days)
    Await.result(receiveList ? 'replyWhenDone, Duration.Inf)
    println("*********************** receive remote finished")

    // add remaing local-only files
    locall.foreach(vf => {
      val cachef = cacherelevant.find(x => x.path == vf.path).getOrElse(null)
      if (cachef != null) cachef.tagged = true // mark
      val cfnew = new ComparedFile(vf, null, cachef, newcache)
      comparedfiles += cfnew
      view.act ! cfnew // send it!
    })
    // add remaining cache-only files for information: local and remote are deleted.
    cacherelevant.filter(vf => !vf.tagged).foreach( vf => {
      val cfnew = new ComparedFile(null, null, vf, newcache)
      comparedfiles += cfnew
      view.act ! cfnew // send it!
    })

    runUIwait { Main.Status.status.value = "ready" }
    view.act ! CompareFinished // send finished!
    sw1.printTime("TTTTTTTTT compared in ")
  }

  def synchronize(cfs: List[ComparedFile]) {
    println("synchronize...")
    runUIwait { Main.Status.status.value = "synchronize..." }
    val sw = new StopWatch
    val swd = new StopWatch
    var iii = cfs.length
    for (cf <- cfs) {
      iii -= 1
      var showit = false
      if (cf.action == A_USELOCAL) { if (cf.flocal.size>10000) showit = true }
      if (cf.action == A_USEREMOTE) { if (cf.fremote.size>10000) showit = true }
      if (swd.getTime > 0.1 || showit) {
        val s = if (cf.flocal != null) cf.flocal.path else if (cf.fremote != null) cf.fremote.path else ""
        runUIwait { // give UI time
          Main.Status.status.value = "synchronize(" + iii + "): " + s
        }
        swd.restart()
      }
      var removecf = true
      try {
        cf.action match {
          case A_MERGE => sys.error("merge not implemented yet!")
          case A_RMLOCAL|A_RMBOTH => { local.deletefile(cf.flocal) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
          case A_RMREMOTE|A_RMBOTH => { remote.deletefile(cf.fremote) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
          case A_USELOCAL => { remote.putfile(cf.flocal) ; Cache.addupdate(cf.flocal) }
          case A_USEREMOTE => { remote.getfile(cf.fremote) ; if (cf.fremote!=cf.fcache) Cache.addupdate(cf.fremote) }
          case A_NOTHING => { if (cf.fcache==null) Cache.addupdate(cf.fremote) }
          case A_CACHEONLY => { Cache.remove(cf.fcache) }
          case _ => removecf = false
        }
      } catch {
        case e: Exception => {
          println("exception:" + e)
          throw new Exception("Exception while synchronizing: \n" + e)
        }
      }
      if (removecf) view.act ! RemoveCF(cf)
    }
    view.act ! 'done
    sw.printTime("TTTTTTTTT synchronized in ")
    runUIwait { Main.Status.status.value = "save cache..." }
    StopWatch.timed("TTTTTTTTT saved cache in ") {
      Cache.saveCache(server.id)
    }
    runUIwait { Main.Status.status.value = "ready" }
  }
  def finish() {
    if (remote != null) remote.finish()
    if (protocol.executeAfter.value != "") {
      import sys.process._
      val res = protocol.executeAfter.value.split("#").toSeq.!
      if (res != 0) {
        throw new Exception("error executing 'after' command!")
      }
    }
  }

}

