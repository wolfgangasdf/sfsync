package sfsync.synchro

import sfsync.Main.Dialog
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.Predef._
import scala.collection.JavaConversions._
import akka.actor.ActorRef
import com.jcraft.jsch
import jsch.ChannelSftp
import sfsync.store.Tools
import java.text.Normalizer
import java.nio.file._
import attribute.FileTime

class cachedFile(path: String, modTime: Long, size: Long) {
}

import sfsync.Helpers._

class LocalConnection extends GeneralConnection {

  def deletefile(what: VirtualFile) {
    Files.delete(Paths.get(remoteBasePath + "/" + what.path))
//    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile) {
    Files.copy(Paths.get(localBasePath + "/" + from.path), Paths.get(remoteBasePath + "/" + from.path), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
  }
  def getfile(from: VirtualFile) {
    Files.copy(Paths.get(remoteBasePath + "/" + from.path), Paths.get(localBasePath + "/" + from.path), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
  }
  // include the subfolder but root "/" is not allowed!
  def listrec(subfolder: String, filterregexp: String, receiver: ActorRef) = {
    val list = new ListBuffer[VirtualFile]()
    // scalax.io is horribly slow, there is an issue filed
    def parseContent(cc: Path, firstTime: Boolean = false) {
      // on mac 10.8 with oracle java 7, filenames are encoded with strange 'decomposed unicode'. grr
      // this is in addition to the bug that LC_CTYPE is not set. grrr
      // don't use cc.getPath directly!!
      val fixedPath = Normalizer.normalize(cc.toString, Normalizer.Form.NFC)
      val strippedPath: String = if (fixedPath == remoteBasePath) "/" else fixedPath.substring(remoteBasePath.length)
      val vf = new VirtualFile(strippedPath, Files.getLastModifiedTime(cc).toMillis, Files.size(cc), if (Files.isDirectory(cc)) 1 else 0)
      if ( !vf.fileName.matches(filterregexp) ) {
        list += vf
        if (receiver != null) receiver ! vf
        if (vf.isDir == 1) {
          val dir = Files.newDirectoryStream(cc)
          for (cc <- dir) parseContent(cc)
        }
      }
      unit()
    }
    val sp = Paths.get(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
    if (Files.exists(sp)) {
      parseContent(sp, firstTime = true)
    } else {
      if (runUIwait(Dialog.showYesNo("Local directory \n" + sp + "\n doesn't exist. Create?")) == true)
        Files.createDirectories(sp)
    }
    if (receiver != null) receiver ! 'done
    list
  }
  def finish() {}
}

class MyURI(var protocol: String, var username: String, var password: String, var host: String, var port: String) {
  val regexinet = new Regex("""(\S+)://(\S+)@(\S+):(\S+)""")
  def this() = this("","","","","")
  def parseString(s: String): Boolean = {
    s match {
      case "file:///" => { protocol = "file" ; true }
      case regexinet(prot1, userinfo, host1, port1) => {
        protocol = prot1
        host = host1
        port = port1
        val uis = userinfo.split(":")
        uis.length match {
          case 1 => { username = uis(0); password = "" }
          case 2 => { username = uis(0); password = uis(1) }
        }
        true
      }
      case _ => { false }
    }
  }
  def toURIString = {
    protocol + "://" + username + ":" + password + "@" + host + ":" + port
  }
  override def toString = {
    s"$protocol,$username,$host,$port"
  }
}
object MyURI {
  def apply(s: String) = {
    val u = new MyURI()
    if (!u.parseString(s))
      throw new RuntimeException("URI in wrong format: " + s)
    u
  }
}


class SftpConnection(var uri: MyURI) extends GeneralConnection {
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
      sftp.put(localBasePath + "/" + from.path, rp)
    sftp.setMtime(rp, (from.modTime/1000).toInt)
  }
  def getfile(from: VirtualFile) {
    val lp = localBasePath + "/" + from.path
    if (from.isDir == 1) {
      Files.createDirectory(Paths.get(lp))
    } else {
      sftp.get(remoteBasePath + "/" + from.path, lp)
    }
    Files.setLastModifiedTime(Paths.get(lp), FileTime.fromMillis(from.modTime))
  }

  def sftpexists(sp: String): ChannelSftp#LsEntry = {
    val xx = sftp.ls(Paths.get(sp).getParent.toString)
    for (obj <- xx) {
      val sftplse = obj.asInstanceOf[ChannelSftp#LsEntry]
      if (sftplse.getFilename == Paths.get(sp).getFileName.toString) {
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
    def parseContent(folder: String) {
      println("parseContent: " + folder)
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
      unit()
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
    println("parsing done")
    if (receiver != null) receiver ! 'done
    list
  }

  import scala.collection.immutable.Map

  class MyUserInfo(val user: String, val password: String) extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    var getPassCount = 0
    def getPassword = {
      println(s"getPassword passcount = $getPassCount")
      getPassCount += 1
      if (getPassCount < 2 && password != "") password
      else runUIwait(Dialog.showInputString("Enter password:")).asInstanceOf[String]
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

  var prvkeypath = ""
  var knownhostspath = ""
  val osname = System.getProperty("os.name")
  if (isMac || isLinux) {
      prvkeypath = System.getProperty("user.home") + "/.ssh/id_dsa"
      knownhostspath = System.getProperty("user.home") + "/.ssh/known_hosts"
  } else println("Can't get private key file, os not supported yet." )
  if (prvkeypath != "") {
    println("prv key: " + prvkeypath)
    var prvkey: Array[Byte] = null
    if (Files.exists(Paths.get(prvkeypath))) prvkey = Files.readAllBytes(Paths.get(prvkeypath))
    if (Files.exists(Paths.get(knownhostspath))) jSch.setKnownHosts(knownhostspath)
    jSch.addIdentity(uri.username,prvkey,null,Array[Byte]())
  }

  var password = uri.password
  if (password != "") {
    if (password.startsWith("##")) { // decode password
      password = Tools.crypto.decrypt(password.substring(2), "bvfxsdfk")
    }
  }
  var session = jSch.getSession(uri.username, uri.host, uri.port.toInt)

  var ui = new MyUserInfo(uri.username, password)
  session.setUserInfo(ui)
  session.connect()
  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]
  sftp.connect(5000)
  if (!sftp.isConnected) {
    throw new Exception("sftp not connected!")
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
  def getfile(from: VirtualFile)
  def putfile(from: VirtualFile)
  def deletefile(what: VirtualFile)
  def listrec(where: String, filterregexp: String, receiver: ActorRef): ListBuffer[VirtualFile]
  def finish()
}

// path below baspath with a leading "/"
class VirtualFile(var path: String, var modTime: Long, var size: Long, var isDir: Int) extends Ordered[VirtualFile] {
  var tagged = false // for cachelist: tagged if local/remote existing, does not need to be added "cacheonly"

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

