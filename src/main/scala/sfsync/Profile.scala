package sfsync.synchro

import Actions._
import util.Logging
import sfsync.store.Cache
import scala.actors._
import Actor._
import scala.concurrent.ops.spawn


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
    } //else { // item not in cache but both present: leave unknown (the only secure thing)
//      action = A_UNKNOWN
//      if (flocal.modTime>fremote.modTime)
//        action = A_USELOCAL
//      else
//      action = A_USEREMOTE
//    }
  }
  assert(action != -9)

}
import scala.collection.mutable.ListBuffer

case object CompareFinished
case class RemoveCF(cf: ComparedFile)

class Profile  (view: Actor,
                localFolder: String,
                protocol: TransferProtocol,
                subfolder: String,
                id: String
                ) extends Logging  {
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null

  def init() {
    cache = Cache.loadCache(id)
    // test local conn
    local = new LocalConnection {
      remoteBasePath = localFolder
    }
    remote = protocol.uri match {
      case s if s.startsWith("sftp://") => new SftpConnection
      case "local" => new LocalConnection
      case _ => { sys.error("wrong protocol URI") }
    }
    remote.localBasePath = localFolder
    remote.remoteBasePath = protocol.basefolder
  }

  def compare() {
    println("***********************list local")
    var locall = local.listrec(subfolder, null)
    println("***********************result:")
    locall.foreach(vf => println(vf))
    println("***********************cache" + cache)
    cache.foreach(vf => println(vf))
    println("***********************receive remote list")
    val receiveList = actor {
      var finished = false
      loop {
        receive {
          case rf: VirtualFile => {
            val cachef = cache.find(x => x.path == rf.path).getOrElse(null)
            val localf = locall.find(x => x.path == rf.path).getOrElse(null)
            locall -= localf
            val cfnew = new ComparedFile(localf, rf, cachef)
            comparedfiles += cfnew
            view ! cfnew // send it to view!
            debug(cfnew)
          }
          case 'done => {
            finished = true
            println("remotelistfinished!")
          }
          case 'replyWhenDone => if (finished) {
            reply('done)
            println("exiting actor profile")
            exit()
          }
        }
      }
    }

    remote.listrec(subfolder, receiveList)
    receiveList !? 'replyWhenDone
    println("*********************** receive remote finished")

    // add remaing remote-only files
    locall.foreach(vf => {
      val cachef = cache.find(x => x.path == vf.path).getOrElse(null)
      val cfnew = new ComparedFile(vf, null, cachef)
      comparedfiles += cfnew
      view ! cfnew // send it!
    })
//    debug("***********************compfiles")
//    comparedfiles.foreach(cf => println(cf))
    view ! CompareFinished // send finished!
  }

  def synchronize(cfs: List[ComparedFile]) {
    for (cf <- cfs) {
      println("***** cf:" + cf)
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
    }
    view ! 'done
    Cache.saveCache(id)
  }
  def finish() {
    remote.finish()
  }

}

