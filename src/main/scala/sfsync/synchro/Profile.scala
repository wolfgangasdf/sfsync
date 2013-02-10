package sfsync.synchro

/*
this runs not in ui thread, so use runUI and runUIwait!
 */

import scala.collection.mutable.ListBuffer
import Actions._
import akka.actor.ActorDSL._
import sfsync.store._
import sfsync.{Main, CompareWindow}
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
}
case object ComparedFile
class ComparedFile(val flocal: VirtualFile, val fremote: VirtualFile, val fcache: VirtualFile) {
  var action: Int = -9
  def isSynced = if (flocal != null) flocal == fremote else false

  override def toString: String = "A=" + action + " local:" + flocal + " remote:" + fremote + " cache:" + fcache
  override def hashCode = action.hashCode() + (if (flocal!=null) flocal.hashCode else 0)
    + (if (fremote!=null) fremote.hashCode else 0) + (if (fcache!=null) fcache.hashCode else 0)
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[ComparedFile] && (this.hashCode() == that.asInstanceOf[ComparedFile].hashCode())
  }

  //  def compare(that: VirtualFile): Int =

  // init with best guess
  if (flocal == null && fremote == null && fcache != null) { // cache only?
    action = A_CACHEONLY
  } else  if (flocal == fremote) { // just equal?
    action = A_NOTHING
  } else if (flocal == null || fremote == null) { // one was deleted or created, which?
    action = A_UNKNOWN // unknown which was deleted
    if (flocal==null && fremote==fcache) {
      action = A_RMREMOTE // locally deleted
    } else if (fremote==null && flocal==fcache) {
      action = A_RMLOCAL // remotely deleted
    } else if (flocal == null && fcache == null) {
      action = A_USEREMOTE // new remote file
    } else if (fremote == null && fcache == null) {
      action = A_USELOCAL // new local file
    }
  } else { // both present, one is modified
    action = A_UNKNOWN
    if (flocal == fcache && fcache != null) { // no local modification
      if (fremote.modTime>fcache.modTime)
        action = A_USEREMOTE // only if remote newer than cache, else unknown
    } else if (fremote == fcache && fcache != null) { // local mod, remote not modified
      if (flocal.modTime > fcache.modTime)  // only if local newer than cache, else unknown
        action = A_USELOCAL
    }
  }
  assert(action != -9)
  //println("cf: " + toString())
}

case class CompareFinished()
case class RemoveCF(cf: ComparedFile)

class Profile  (view: CompareWindow, server: Server, protocol: Protocol, subfolder: SubFolder) {
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  var cache: ListBuffer[VirtualFile] = null
  var cacherelevant: ListBuffer[VirtualFile] = null // only below subdir
  var local: GeneralConnection = null
  var remote: GeneralConnection = null

  var remotecnt = 0

  def init() {
    runUIwait { view.statusBar.status.text = "load cached files..." }
    cache = Cache.loadCache(server.id)
    // TODO: why doesn't the implicit from Helpers work here and i need subfolder.value???
    if (!subfolder.subfolders.isEmpty) {
      cacherelevant = cache.filter(cf => cf.path.startsWith(subfolder.subfolders.collect( { case s: String => "/" + s + "/"})))
    } else
      cacherelevant = cache

    if (protocol.executeBefore.value != "") {
      runUIwait { view.statusBar.status.text = "execute 'before'..." }
      import sys.process._
      val res = protocol.executeBefore.value.split("#").toSeq.!
      if (res != 0) {
        sys.error("error executing 'before' command!")
      }
    }

    runUIwait { view.statusBar.status.text = "ready" }

    local = new LocalConnection {
      remoteBasePath = server.localFolder.value
    }
    val uri = new java.net.URI(protocol.protocoluri)
    println("scheme = " + uri.getScheme)
    runUIwait { view.statusBar.status.text = "ini remote connection..." }
    remote = uri.getScheme match {
      case "sftp" => new SftpConnection(uri)
      case "file" => new LocalConnection
      case _ => { println("wrong protocol URI scheme: " + uri.getScheme); sys.exit(1) }
    }
    runUIwait { view.statusBar.status.text = "ready" }
    remote.localBasePath = server.localFolder.value
    remote.remoteBasePath = protocol.protocolbasefolder
  }

  def compare() {
    runUIwait { view.statusBar.status.text = "list local files..." }
    println("***********************list local")

    var locall = new ListBuffer[VirtualFile]()
    StopWatch.timed("loaded local list in ") {
      for (sf <- subfolder.subfolders) locall ++= local.listrec(sf, server.filterRegexp, null)
    }
    runUIwait { view.statusBar.local.text = locall.length.toString }
//    println("***********************result:")
//    locall.foreach(vf => println(vf))
//    println("***********************cache" + cache)
//    cache.foreach(vf => println(vf))
    runUIwait { view.statusBar.status.text = "compare to remote files..." }
    println("***********************receive remote list")

    val receiveList = actor(Main.system)(new Act {
      var finished = false
      become {
        case rf: VirtualFile => {
          remotecnt += 1
          if (remotecnt % 200 == 0) runUIwait { // give UI time
            view.statusBar.remote.text = remotecnt.toString
          }
          val cachef = cacherelevant.find(x => x.path == rf.path).getOrElse(null)
          if (cachef != null) cachef.tagged = true // mark
          val localf = locall.find(x => x.path == rf.path).getOrElse(null)
          locall -= localf
          val cfnew = new ComparedFile(localf, rf, cachef)
          if (!server.skipEqualFiles.value || rf != localf) { // TODO test this!!!
            comparedfiles += cfnew
            view.act ! cfnew // send it to view!
          } else {
            // for save cache later, only in case 'synchronize' is pressed it's saved!
            if (cachef == null) Cache.addupdate(rf) // it should work, but TODO test this!!!
          }
        }
        case 'done => {
          finished = true
          println("receiveList: remotelistfinished!")
          runUI {
            view.statusBar.remote.text = remotecnt.toString
          }
        }
        case 'replyWhenDone => if (finished) {
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
      val cfnew = new ComparedFile(vf, null, cachef)
      comparedfiles += cfnew
      view.act ! cfnew // send it!
    })
    // add remaining cache-only files for information: local and remote are deleted.
    cacherelevant.filter(vf => !vf.tagged).foreach( vf => {
      val cfnew = new ComparedFile(null, null, vf)
      comparedfiles += cfnew
      view.act ! cfnew // send it!
    })

    runUIwait { view.statusBar.status.text = "ready" }
    view.act ! CompareFinished // send finished!
    sw1.printTime("TTTTTTTTT compared in ")
  }

  def synchronize(cfs: List[ComparedFile]) {
    println("synchronize...")
    runUIwait { view.statusBar.status.text = "synchronize..." }
    println("aaaaa...")
    val sw = new StopWatch
    var iii = cfs.length
    for (cf <- cfs) {
      iii -= 1
      var removecf = true
      cf.action match {
        case A_MERGE => sys.error("merge not implemented yet!")
        case A_RMLOCAL => { local.deletefile(cf.flocal) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
        case A_RMREMOTE => { remote.deletefile(cf.fremote) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
        case A_USELOCAL => { remote.putfile(cf.flocal) ; Cache.addupdate(cf.flocal) }
        case A_USEREMOTE => { remote.getfile(cf.fremote) ; if (cf.fremote!=cf.fcache) Cache.addupdate(cf.fremote) }
        case A_NOTHING => { if (cf.fcache==null) Cache.addupdate(cf.fremote) }
        case A_CACHEONLY => { Cache.remove(cf.fcache) }
        case _ => removecf = false
      }
      if (removecf) view.act ! RemoveCF(cf)
      if (iii % 100 == 0) runUIwait { // give UI time
        view.statusBar.status.text = "synchronize " + iii
      }
    }
    view.act ! 'done
    sw.printTime("TTTTTTTTT synchronized in ")
    runUIwait { view.statusBar.status.text = "save cache..." }
    StopWatch.timed("TTTTTTTTT saved cache in ") {
      Cache.saveCache(server.id)
    }
    runUIwait { view.statusBar.status.text = "ready" }
  }
  def finish() {
    if (remote != null) remote.finish()
    if (protocol.executeAfter.value != "") {
      import sys.process._
      val res = protocol.executeAfter.value.split("#").toSeq.!
      if (res != 0) {
        sys.error("error executing 'after' command!")
      }
    }
  }

}
