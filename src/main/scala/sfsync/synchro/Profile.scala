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
import scala.concurrent.ExecutionContext.Implicits.global
import org.squeryl.Session

class TransferProtocol (
  var uri: String,
  var basefolder: String
)

object Actions {
  val A_UNCHECKED = -99
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
case class addFile(vf: VirtualFile, islocal: Boolean)

class Profile  (view: FilesView, server: Server, protocol: Protocol, subfolder: SubFolder) {
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null

  def init() {
    if (protocol.executeBefore.value != "") {
      runUIwait { Main.Status.status.value = "execute 'before'..." }
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
    println("puri = " + protocol.protocoluri.value)
    val uri = MyURI(protocol.protocoluri.value)
    println("proto = " + uri.protocol)
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

    // reset table
    println("resetting table...")
    import org.squeryl.PrimitiveTypeMode.transaction
    import org.squeryl.PrimitiveTypeMode._
    var tmp = false
    transaction {
      // get all:
      // val q = from(MySchema.files)(s=>select(s))
      val q = from(MySchema.files)(s=>select(s)) //.where(se => se.path.regex("/" + sf + "/.*"))
      MySchema.files.update(q.map(a =>{
        tmp = false
        for (sf <- subfolder.subfolders)if (a.path.startsWith("/" + sf + "/")) tmp = true
        if (tmp) {
          a.lSize = -1
          a.rSize = -1
          a.relevant = true
        } else {
          a.relevant = false
        }
        a
      }))
    }
    runUI { view.updateSyncEntries() }
    var receiveSession: Session = null
    // the receive actor
    val sw = new StopWatch // for UI update
    val receiveList = actor(Main.system)(new Act {
      var finished = 2*subfolder.subfolders.length // cowntdown for lists
      become {
        case addFile(vf, islocal) => {
//          println("  received " + vf)
          if (sw.getTime > 1) {
            runUIwait { // give UI time
              Main.Status.status.value = "list " + vf.path
              view.updateSyncEntries()
            }
            sw.restart()
          }
          if (receiveSession==null) {
            receiveSession = CacheDB.getSession
            println("created receivesession " + receiveSession + " in Thread " + Thread.currentThread().getId)
          }
          using (receiveSession) {
            val q = MySchema.files.where(se => (se.path === vf.path))
            if (q.size == 0) { // new entry
              val senew = new SyncEntry(vf.path, A_UNCHECKED, if (islocal) vf.modTime else 0, if (islocal) vf.size else -1,
                if (!islocal) vf.modTime else 0, if (!islocal) vf.size else 0,0,-1,true)
              MySchema.files.insert(senew)
            } else {
              val se = q.single
              if (islocal) { se.lTime = vf.modTime ; se.lSize = vf.size }
              else         { se.rTime = vf.modTime ; se.rSize = vf.size }
              MySchema.files.update(se)
            }
          }
        }
        case 'done => {
          finished -= 1
          if (finished == 0) {
            println("receiveList: remotelistfinished!")
          }
        }
        case 'replyWhenDone => if (finished==0) {
          sender ! 'done
          println("exit actor receiveList")
          context.stop(self)
        }
      }
    })

    runUIwait { Main.Status.status.value = "list local files..." }
    println("***********************list local")
    future {
      for (sf <- subfolder.subfolders) local.listrec(sf, server.filterRegexp, receiveList)
    }
//    runUIwait { Main.Status.local.value = locall.length.toString }

    runUIwait { Main.Status.status.value = "list remote files..." }
    println("***********************list remote")
    // TODO spawn in new proc?
    future {
      subfolder.subfolders.map( sf => remote.listrec(sf, server.filterRegexp, receiveList) )
    }
    implicit val timeout = Timeout(36500 days)
    println("*********************** wait until all received...")
    Await.result(receiveList ? 'replyWhenDone, Duration.Inf)
    println("*********************** list finished")

    receiveSession.close

    runUIwait {
      view.updateSyncEntries()
      Main.Status.status.value = "ready"
    }
    view.act ! CompareFinished // send finished!
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
            // TODO
//          case A_MERGE => sys.error("merge not implemented yet!")
//          case A_RMLOCAL|A_RMBOTH => { local.deletefile(cf.flocal) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
//          case A_RMREMOTE|A_RMBOTH => { remote.deletefile(cf.fremote) ; if (cf.fcache!=null) Cache.remove(cf.fcache) }
//          case A_USELOCAL => { remote.putfile(cf.flocal) ; Cache.addupdate(cf.flocal) }
//          case A_USEREMOTE => { remote.getfile(cf.fremote) ; if (cf.fremote!=cf.fcache) Cache.addupdate(cf.fremote) }
//          case A_NOTHING => { if (cf.fcache==null) Cache.addupdate(cf.fremote) }
//          case A_CACHEONLY => { Cache.remove(cf.fcache) }
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

