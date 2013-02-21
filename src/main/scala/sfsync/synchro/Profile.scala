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
import org.squeryl.PrimitiveTypeMode._
//import sfsync.synchro.addFile

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
  val ALLACTIONS = List(-99,-1,0,1,2,3,4,5,6,7)
}

case class addFile(vf: VirtualFile, islocal: Boolean)

class Profile  (view: FilesView, server: Server, protocol: Protocol, subfolder: SubFolder) {
  var cache: ListBuffer[VirtualFile] = null
  var local: GeneralConnection = null
  var remote: GeneralConnection = null

  def init() {
    view.updateSyncButton(false)
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
    var cacheall = false
    for (sf <- subfolder.subfolders) if (sf == "") cacheall = true
    transaction {
      val q = from(MySchema.files)(s=>select(s))
      MySchema.files.update(q.map(a =>{
        var tmp = cacheall
        if (!cacheall) for (sf <- subfolder.subfolders) if (a.path.startsWith("/" + sf + "/")) tmp = true
        if (tmp) {
          a.action = A_UNKNOWN
          a.lSize = -1; a.lTime = 0
          a.rSize = -1; a.rTime = 0
          a.relevant = true
        } else {
          a.relevant = false
        }
        a
      }))
    }
    runUIwait { view.updateSyncEntries() }
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
                if (!islocal) vf.modTime else 0, if (!islocal) vf.size else -1,0,-1,true)
              MySchema.files.insert(senew)
              //println("added new " + senew)
            } else {
              val se = q.single
              if (islocal) { se.lTime = vf.modTime ; se.lSize = vf.size }
              else         { se.rTime = vf.modTime ; se.rSize = vf.size }
              MySchema.files.update(se)
              //println("updated   " + se)
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
          receiveSession.close
          sender ! 'done
          println("exit actor receiveList")
//          context.stop(self)
        } else sender ! 'notyet
      }
    })

    runUIwait { Main.Status.status.value = "list local files..." }
    println("***********************list local")
    future {
      for (sf <- subfolder.subfolders) local.listrec(sf, server.filterRegexp, receiveList)
    }

    runUIwait { Main.Status.status.value = "list remote files..." }
    println("***********************list remote")
    future {
      subfolder.subfolders.map( sf => remote.listrec(sf, server.filterRegexp, receiveList) )
    }
    implicit val timeout = Timeout(36500 days)
    println("*********************** wait until all received...")
    while (Await.result(receiveList ? 'replyWhenDone, Duration.Inf) != 'done) { Thread.sleep(100) }
    println("*********************** list finished")

    // init with best guess
    println("*********************** ini with best guess")
    transaction {
      val q = MySchema.files.where(se => se.relevant === true)
      //println("  have size=" + q.size)
      MySchema.files.update(q.map(se => se.iniAction(!server.didInitialSync.value)))
    }


    println("*********************** compare: finish up")
    runUIwait {
      view.updateSyncEntries()
      view.updateSyncButton(true)
      Main.Status.status.value = "ready"
    }
  }

  def synchronize() {
    println("synchronize...")
    runUIwait { Main.Status.status.value = "synchronize..." }
    val sw = new StopWatch
    val swd = new StopWatch
    var iii = 0
    transaction {
      val q = from (MySchema.files) (se =>
        where(se.relevant === true)
        select(se)
        orderBy(se.path desc) // this allows deletion of dirs!
      )
      MySchema.files.update(q.map(se => {
        iii += 1

        var showit = false
        if (se.action == A_USELOCAL) { if (se.lSize>10000) showit = true }
        if (se.action == A_USEREMOTE) { if (se.rSize>10000) showit = true }
        if (swd.getTime > 0.1 || showit) {
          runUIwait { // give UI time
            Main.Status.status.value = "synchronize(" + iii + "): " + se.path
          }
          swd.restart()
        }
        var removecf = true
        try {
          se.action match {
            case A_MERGE => throw new Exception("merge not implemented yet!")
            case A_RMLOCAL|A_RMBOTH => { local.deletefile(se.path,se.lTime) ; se.delete = true }
            case A_RMREMOTE|A_RMBOTH => { remote.deletefile(se.path, se.rTime) ; se.delete = true }
            case A_USELOCAL => { remote.putfile(se.path, se.lTime) ; se.rTime=se.lTime; se.rSize=se.lTime; se.cSize = se.lSize; se.cTime = se.lTime }
            case A_USEREMOTE => { remote.getfile(se.path, se.rTime) ; se.lTime=se.rTime; se.lSize=se.rTime; se.cSize = se.rSize; se.cTime = se.rTime }
            case A_NOTHING => { se.cSize = se.rSize; se.cTime = se.rTime }
            case A_CACHEONLY => { se.delete = true }
            case _ => removecf = false
          }
        } catch {
          case e: Exception => {
            println("exception:" + e)
            throw new Exception("Exception while synchronizing: \n" + e)
          }
        }
        se
      })) // update
      // update cache: remove removed files
      MySchema.files.deleteWhere(se => se.delete === true)
    }

    server.didInitialSync.value = true

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

