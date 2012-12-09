package sfsync.synchro

import Actions._
import util.Logging
import sfsync.store.Cache
import scala.actors._
import Actor._


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
  def equals(obj: ComparedFile): Boolean = {
//    println("eq: " + (this.hashCode == obj.hashCode) + "     obj=" + obj + " this=" + this.toString)
    this.hashCode == obj.hashCode
  }


  //  def compare(that: VirtualFile): Int =

  // init with best guess
  if (flocal == fremote) { // just equal?
    action = A_NOTHING
  } else if (flocal == null || fremote == null) { // one was deleted, which?
    if (fcache==null) {
      action = A_UNKNOWN // unknown which was deleted
    } else {
      if (flocal==null && fremote==fcache) {
        action = A_RMREMOTE
      } else if (fremote==null && flocal==fcache) {
        action = A_RMLOCAL
      } else {
        action = A_UNKNOWN // one was deleted and the other modified: don't know
      }
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

case object CompareFinished
case class RemoveCF(cf: ComparedFile)

class Profile  (view: Actor,
                localFolder: String,
                protocol: TransferProtocol,
                subfolder: String,
                id: String
                ) extends Logging /*with Actor*/ {
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  var cache = Cache.loadCache(id)
  // test local conn
  var local = new LocalConnection {
    remoteBasePath = localFolder
  }
  var remote = protocol.uri match {
    case s if s.startsWith("sftp://") => new SftpConnection
    case "local" => new LocalConnection
    case _ => { sys.error("wrong protocol URI") }
  }
  remote.localBasePath = localFolder
  remote.remoteBasePath = protocol.basefolder

  debug("***********************local")
  var locall = local.listrec(subfolder, null)
  locall.foreach(vf => debug(vf))

  debug("***********************cache")
  cache.foreach(vf => debug(vf))
  debug("***********************")
  debug("***********************receive remote list")
  val receiveList = actor {
    var finished = false
    loop {
      println("before")
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
        case 'replyWhenDone => if (finished) { reply('done) ; exit() }
      }
    }
    println("a2")
  }
  remote.listrec(subfolder, receiveList)
  receiveList !? 'replyWhenDone
  debug("*********************** receive remote finished")

  // add remaing remote-only files
  locall.foreach(vf => {
    val cachef = cache.find(x => x.path == vf.path).getOrElse(null)
    val cfnew = new ComparedFile(vf, null, cachef)
    comparedfiles += cfnew
    view ! cfnew // send it!
  })
  debug("***********************compfiles")
  comparedfiles.foreach(cf => println(cf))
  view ! CompareFinished // send finished!

  def synchronize(cfs: List[ComparedFile]) {
    for (cf <- cfs) {
      println("***** cf:" + cf)
      var removecf = true
      cf.action match {
        case A_MERGE => sys.error("merge not implemented yet!")
        case A_RMLOCAL => local.deletefile(cf.flocal)
        case A_RMREMOTE => remote.deletefile(cf.fremote)
        case A_USELOCAL => remote.putfile(cf.flocal)
        case A_USEREMOTE => remote.getfile(cf.fremote)
        case A_NOTHING => {}
        case _ => removecf = false
      }
      if (removecf) view ! RemoveCF(cf)
    }
    view ! 'done
  }
  def finish() {
    remote.finish()
  }

}

