package sfsync.synchro

/*
this runs not in ui thread, so use runUI and runUIwait!
 */

import Actions._
import scala.actors._
import Actor._
import sfsync.store._
import sfsync.CompareWindow
import sfsync.Helpers._
import util.Profiling._


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
}
case object ComparedFile
class ComparedFile(var flocal: VirtualFile, var fremote: VirtualFile, var fcache: VirtualFile) {
  var action: Int = -9
  def isSynced = flocal.equals(fremote)

  override def toString: String = "A=" + action + " local:" + flocal + " remote:" + fremote + " cache:" + fcache
  override def hashCode = action.hashCode() + (if (flocal!=null) flocal.hashCode else 0)
    + (if (fremote!=null) fremote.hashCode else 0) + (if (fcache!=null) fcache.hashCode else 0)
  override def equals(that: Any): Boolean = {
    that.isInstanceOf[ComparedFile] && (this.hashCode() == that.asInstanceOf[ComparedFile].hashCode())
  }

  //  def compare(that: VirtualFile): Int =

  // init with best guess
  if (flocal == fremote) { // just equal?
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

}
import scala.collection.mutable.ListBuffer

case object CompareFinished
case class RemoveCF(cf: ComparedFile)

class Profile  (view: CompareWindow, server: Server, protocol: Protocol, subfolder: SubFolder) {
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null

  var remotecnt = 0

  def init() {
    runUIwait { view.statusBar.status.text = "load cached files..." }
    cache = Cache.loadCache(server.id)
    runUIwait { view.statusBar.status.text = "ready" }

    local = new LocalConnection {
      remoteBasePath = server.localFolder
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
    remote.localBasePath = server.localFolder
    remote.remoteBasePath = protocol.protocolbasefolder
  }

  def compare() {
    runUIwait { view.statusBar.status.text = "list local files..." }
    println("***********************list local")
    var locall = timed(printTime("loaded local list in ")) {
      local.listrec(subfolder.subfolder, server.filterRegexp, null)
    }
    runUIwait { view.statusBar.local.text = locall.length.toString }
//    println("***********************result:")
//    locall.foreach(vf => println(vf))
//    println("***********************cache" + cache)
//    cache.foreach(vf => println(vf))
    runUIwait { view.statusBar.status.text = "compare to remote files..." }
    println("***********************receive remote list")
    val receiveList = actor {
      var finished = false
      loop {
        receive {
          case rf: VirtualFile => {
            remotecnt += 1
            val cachef = cache.find(x => x.path == rf.path).getOrElse(null)
            val localf = locall.find(x => x.path == rf.path).getOrElse(null)
            locall -= localf
            val cfnew = new ComparedFile(localf, rf, cachef)
            comparedfiles += cfnew
            view ! cfnew // send it to view!
            if (remotecnt % 100 == 0) runUIwait { // give UI time
              view.statusBar.remote.text = remotecnt.toString
            }
//            println(cfnew)
          }
          case 'done => {
            finished = true
            println("remotelistfinished!")
            runUI { view.statusBar.remote.text = remotecnt.toString }
          }
          case 'replyWhenDone => if (finished) {
            reply('done)
            println("exiting actor profile")
            exit()
          }
        }
      }
    }

    timed(printTime("compared in ")){
      remote.listrec(subfolder.subfolder, server.filterRegexp, receiveList)
      receiveList !? 'replyWhenDone
      println("*********************** receive remote finished")

      // add remaing local-only files
      locall.foreach(vf => {
        val cachef = cache.find(x => x.path == vf.path).getOrElse(null)
        val cfnew = new ComparedFile(vf, null, cachef)
        comparedfiles += cfnew
        view ! cfnew // send it!
      })
  //    debug("***********************compfiles")
  //    comparedfiles.foreach(cf => println(cf))
      runUIwait { view.statusBar.status.text = "ready" }
      view ! CompareFinished // send finished!
    }
  }

  def synchronize(cfs: List[ComparedFile]) {
    println("synchronize...")
    runUIwait { view.statusBar.status.text = "synchronize..." }
    println("aaaaa...")
    timed(printTime("synchronized in ")) {
      var iii = cfs.length
      for (cf <- cfs) {
        iii -= 1
  //      println("***** cf:" + cf)
        var removecf = true
        cf.action match {
          case A_MERGE => sys.error("merge not implemented yet!")
          case A_RMLOCAL => local.deletefile(cf.flocal)
          case A_RMREMOTE => { remote.deletefile(cf.fremote) ; if (cache.contains(cf.fremote)) Cache.remove(cf.fremote) }
          case A_USELOCAL => { remote.putfile(cf.flocal) ; Cache.addupdate(cf.flocal) }
          case A_USEREMOTE => { remote.getfile(cf.fremote) ; if (!cache.contains(cf.fremote)) Cache.addupdate(cf.fremote) }
          case A_NOTHING => { if (!cache.contains(cf.fremote)) Cache.addupdate(cf.fremote) }
          case _ => removecf = false
        }
        if (removecf) view ! RemoveCF(cf)
        if (iii % 100 == 0) runUIwait { // give UI time
          view.statusBar.status.text = "synchronize " + iii
        }
      }
      view ! 'done
    }
    runUIwait { view.statusBar.status.text = "save cache..." }
    timed(printTime("saved cache in ")) {
      Cache.saveCache(server.id)
    }
    runUIwait { view.statusBar.status.text = "ready" }
  }
  def finish() {
    if (remote != null) remote.finish()
  }

}

