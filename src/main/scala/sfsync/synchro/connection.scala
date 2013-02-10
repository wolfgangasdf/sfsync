package sfsync.synchro

import sfsync.Main.Dialog
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.Predef._
import scala.collection.JavaConversions._
import akka.actor.ActorRef
import scalax.file.Path
import com.jcraft.jsch
import jsch.{SftpATTRS, ChannelSftp}

class cachedFile(path: String, modTime: Long, size: Long) {
}

import sfsync.Helpers._

class LocalConnection extends GeneralConnection {

  def deletefile(what: VirtualFile) {
    Path.fromString(remoteBasePath + "/" + what.path).delete(force = true)
//    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile) {
    Path.fromString(localBasePath + "/" + from.path).copyTo(Path.fromString(remoteBasePath + "/" + from.path),replaceExisting = true)
  }
  def getfile(from: VirtualFile) {
    Path.fromString(remoteBasePath + "/" + from.path).copyTo(Path.fromString(localBasePath + "/" + from.path),replaceExisting = true)
  }
  // include the subfolder but root "/" is not allowed!
  def listrec(subfolder: String, filterregexp: String, receiver: ActorRef) = {
    val list = new ListBuffer[VirtualFile]()
    // scalax.io is horribly slow, there is an issue filed
    def parseContent(cc: java.io.File, firstTime: Boolean = false) : Unit = {
      val strippedPath: String = if (cc.getPath == remoteBasePath) "/" else cc.getPath.substring(remoteBasePath.length)
      val vf = new VirtualFile(strippedPath, cc.lastModified(), cc.length, if (cc.isDirectory) 1 else 0)
      if ( !vf.fileName.matches(filterregexp) ) {
        list += vf
        if (receiver != null) receiver ! vf
        if (vf.isDir == 1) for (cc <- cc.listFiles()) parseContent(cc)
      }
      getUnit
    }
    val sp = Path.fromString(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
    val spf = new java.io.File(sp.path)
    if (spf.exists) {
      parseContent(spf, firstTime = true)
    } else {
      if (runUIwait(Dialog.showYesNo("Local directory \n" + spf + "\n doesn't exist. Create?")) == true)
        spf.mkdir()
    }
    if (receiver != null) receiver ! 'done
    list
  }
  def finish() {}
}

class SftpConnection(var uri: java.net.URI) extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    if (what.isDir == 1)
      sftp.rmdir(remoteBasePath + "/" + what.path)
    else
      sftp.rm(remoteBasePath + "/" + what.path)
  }
  def putfile(from: VirtualFile) {
    val rp = remoteBasePath + "/" + from.path
    if (from.isDir == 1)
      sftp.mkdir(rp)
    else
      sftp.put(localBasePath + "/" + from.path, rp) // TODO: progressmonitor!
    sftp.setMtime(rp, (from.modTime/1000).toInt)
  }
  def getfile(from: VirtualFile) {
    val lp = localBasePath + "/" + from.path
    sftp.get(remoteBasePath + "/" + from.path, lp) // TODO: progressmonitor!
    Path.fromString(lp).lastModified = from.modTime
  }

  def sftpexists(sp: String): ChannelSftp#LsEntry = {
    val xx = sftp.ls(Path.fromString(sp).parent.get.path)
    for (obj <- xx) {
      val sftplse = obj.asInstanceOf[ChannelSftp#LsEntry]
      if (sftplse.getFilename == Path.fromString(sp).name) {
        return sftplse
      }
    }
    null
  }

  def listrec(subfolder: String, filterregexp: String, receiver: ActorRef) = {
    val list = new ListBuffer[VirtualFile]()
    def VFfromLse(fullFilePath: String, lse: ChannelSftp#LsEntry) = {
      new VirtualFile {
        path=(fullFilePath).substring(remoteBasePath.length)
        if (path == "") path = "/"
        modTime = lse.getAttrs.getMTime.toLong * 1000
        size = lse.getAttrs.getSize
        isDir = if (lse.getAttrs.isDir) 1 else 0
      }
    }
    def parseContent(folder: String): Unit = {
      println("parsing " + folder )
      val xx = sftp.ls(folder)
      val tmp = new ListBuffer[ChannelSftp#LsEntry]
      for (obj <- xx ) { tmp += obj.asInstanceOf[ChannelSftp#LsEntry] } // doesn't work otherwise!
      val ord = new Ordering[ChannelSftp#LsEntry]() { def compare(l: ChannelSftp#LsEntry, r: ChannelSftp#LsEntry) = l.getFilename compare r.getFilename }
      for (obj <- tmp.sorted(ord) ) {
        if (!obj.getFilename.equals(".") && !obj.getFilename.equals("..")) {
          val fullFilePath = folder + "/" + obj.getFilename
          val vf = VFfromLse(fullFilePath, obj)
          if ( !vf.fileName.matches(filterregexp) ) {
            list += vf
            if (receiver != null) receiver ! vf
            if (obj.getAttrs.isDir) {
              parseContent(fullFilePath)
            }
          }
        }
      }
      getUnit
    }
    println("searching " + remoteBasePath + "/" + subfolder)
    val sp = remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder
    val sftpsp = sftpexists(sp)
    if (sftpsp != null) { // not nice, copied basically from above. but no other way
      val vf = VFfromLse(sp, sftpsp)
      if ( !vf.fileName.matches(filterregexp) ) {
        list += vf
        if (receiver != null) receiver ! vf
        if (sftpsp.getAttrs.isDir) {
          parseContent(sp)
        }
      }
    }
//    else {
//      runUIwait(Dialog.showMessage("creating sftp directory " + sp + " ..."))
//      sftp.mkdir(sp)
//    }

    println("parsing done")
    if (receiver != null) receiver ! 'done
    list
  }

  import scala.collection.immutable.Map

  class MyUserInfo extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    def getPassword : String = {
      val res = runUIwait(Dialog.showInputString("Enter password:"))
      res.asInstanceOf[String]
    }
    def promptYesNo(str: String) : Boolean = {
      runUIwait(Dialog.showYesNo(str)) == true
    }

    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null

    def getPassphrase: String = ""

    def promptPassword(message: String): Boolean = { println("prompt pwd") ; true }

    def promptPassphrase(message: String): Boolean = { println("prompt pwd") ; true }

    def showMessage(message: String) { runUIwait(Dialog.showMessage(message)) }
  }

  class MyJschLogger extends jsch.Logger {
    val name = Map(jsch.Logger.DEBUG -> "DEBUG: ",
      jsch.Logger.INFO -> "INFO: ",
      jsch.Logger.WARN -> "WARN: ",
      jsch.Logger.ERROR -> "ERROR: ",
      jsch.Logger.FATAL -> "FATAL: ")

    def isEnabled(level: Int) = true

    def log(level: Int, message: String) {
      println(name(level) + message)
    }

  }

  // init

  jsch.JSch.setLogger(new MyJschLogger)

  var jSch = new jsch.JSch

  println("urxi=" + uri.getScheme)

//  val prvkey: Array[Byte] = scalax.file.Path.fromString("/Users/wolle/.ssh/id_dsa").bytes.toArray
  var prvkeypath = ""
  var knownhostspath = ""
  val osname = System.getProperty("os.name")
  println("osname=" + osname)
  osname match {
    case "Mac OS X" => {
      println("mac")
      prvkeypath = System.getProperty("user.home") + "/.ssh/id_dsa"
      knownhostspath = System.getProperty("user.home") + "/.ssh/known_hosts"
    }
    case _ => { println("not supported:" +  osname) ; sys.exit(1) }
  }
  println("prv key: " + prvkeypath)
  var prvkey: Array[Byte] = null
  if (Path.fromString(prvkeypath).exists) prvkey = Path.fromString(prvkeypath).bytes.toArray
  if (Path.fromString(knownhostspath).exists) jSch.setKnownHosts(knownhostspath)

  jSch.addIdentity(uri.getUserInfo,prvkey,null,Array[Byte]())
  var session = jSch.getSession(uri.getUserInfo, uri.getHost, uri.getPort)

  var ui = new MyUserInfo
  session.setUserInfo(ui)
  session.connect()
  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]
  sftp.connect(5000)
  if (!sftp.isConnected) {
    sys.error("sftp not connected!")
  }

  def finish() {
    if (sftp.isConnected) sftp.disconnect()
    if (session.isConnected) session.disconnect()
  }
}

trait GeneralConnection {
//  var conntype: ConnType.Value
  var localBasePath: String = ""
  var remoteBasePath: String = ""
  var filterregex: Regex = new Regex(""".*""")
  // TODO: time diff thing
  def getfile(from: VirtualFile)
  def putfile(from: VirtualFile)
  def deletefile(what: VirtualFile)
  def listrec(where: String, filterregexp: String, receiver: ActorRef): ListBuffer[VirtualFile]
  def finish()
}

// path below baspath with a leading "/"
class VirtualFile(var path: String, var modTime: Long, var size: Long, var isDir: Int) extends Ordered[VirtualFile] {
  var tagged = false // for cachelist: tagged if local/remote existing

  def this() = this("",0,0,0)
//  def getPathString = if (path == "") "<root>" else path
  def fileName : String = if (path == "/") "/" else path.split("/").last
  override def toString: String = "["+path+"]:"+modTime+","+size

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[VirtualFile] && (this.hashCode() == that.asInstanceOf[VirtualFile].hashCode())
  }
  override def hashCode = {
    if (isDir==1)
      path.hashCode + isDir.hashCode
    else
      path.hashCode + isDir.hashCode + modTime.hashCode + size.hashCode
  }

  def compare(that: VirtualFile): Int = path.compare(that.path)
}

