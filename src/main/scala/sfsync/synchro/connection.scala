package sfsync.synchro

import sfsync.Main.Dialog
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.Predef._
import scala.collection.JavaConversions._
import akka.actor.ActorRef
import com.jcraft.jsch
import com.jcraft.jsch.{SftpException, ChannelSftp}
import sfsync.store.Tools
import java.text.Normalizer
import java.nio.file._
import attribute.FileTime
import sfsync.util.Logging

class cachedFile(path: String, modTime: Long, size: Long) {
}

import sfsync.Helpers._

class LocalConnection(isLocal: Boolean) extends GeneralConnection(isLocal) {

  def deletefile(what: String, mtime: Long) {
    val (cp, _) = checkIsDir(what)
    Files.delete(Paths.get(remoteBasePath + "/" + cp))
    // TODO add same thing as for sftp
//    debug("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: String, mtime: Long) {
    val (cp, _) = checkIsDir(from)
    Files.copy(Paths.get(localBasePath + "/" + cp), Paths.get(remoteBasePath + "/" + cp), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
  }
  def getfile(from: String, mtime: Long) {
    val (cp, _) = checkIsDir(from)
    Files.copy(Paths.get(remoteBasePath + "/" + cp), Paths.get(localBasePath + "/" + cp), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
  }

  // include the subfolder but root "/" is not allowed!
  def list(subfolder: String, filterregexp: String, receiver: ActorRef, recursive: Boolean, viaActor: Boolean) = {
    debug(s"listrec(rbp=$remoteBasePath sf=${subfolder} rec=$recursive) in thread ${Thread.currentThread().getId}")
    val reslist = new ArrayBuffer[VirtualFile]
    // scalax.io is horribly slow, there is an issue filed
    def parseContent(cc: Path, goDeeper: Boolean) {
      // on mac 10.8 with oracle java 7, filenames are encoded with strange 'decomposed unicode'. grr
      // this is in addition to the bug that LC_CTYPE is not set. grrr
      // don't use cc.getPath directly!!
      val fixedPath = Normalizer.normalize(cc.toString, Normalizer.Form.NFC)
      var strippedPath: String = if (fixedPath == remoteBasePath) "/" else fixedPath.substring(remoteBasePath.length)
      if (Files.isDirectory(cc) && strippedPath != "/") strippedPath += "/"
      val vf = new VirtualFile(strippedPath, Files.getLastModifiedTime(cc).toMillis, Files.size(cc))
      if ( !vf.fileName.matches(filterregexp)) {
        if (stopRequested) return
//        debug("sending " + vf)
        if (viaActor) receiver ! addFile(vf, isLocal) else reslist += vf
        if (Files.isDirectory(cc) && goDeeper ) {
          val dir = Files.newDirectoryStream(cc)
          for (cc <- dir) parseContent(cc, goDeeper = recursive)
          dir.close()
        }
      }
      unit()
    }
    val sp = Paths.get(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
    if (Files.exists(sp)) {
      parseContent(sp, goDeeper = true)
    } else {
      if (runUIwait(Dialog.showYesNo("Local directory \n" + sp + "\n doesn't exist. Create?")) == true)
        Files.createDirectories(sp)
    }
    if (viaActor) {receiver ! 'done ; null} else reslist
  }

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


class SftpConnection(isLocal: Boolean, var uri: MyURI) extends GeneralConnection(isLocal) {
  def deletefile(what: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(what)
    if (isdir) {
      try {
        sftp.rmdir(remoteBasePath + "/" + cp)
      } catch {
        case sftpe: SftpException => { // unfortunately only "Failure"
          val xx = sftp.ls(remoteBasePath + "/" + cp)
          if (xx.length > 0) {
            val tmp = new ListBuffer[ChannelSftp#LsEntry]
            for (obj <- xx ) {
              val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
              lse.getFilename match {
                case "." | ".." => {}
                case s => tmp += lse
              }
            }
            val msg = s"Directory \n $cp \n not empty, content:\n" + tmp.map(a => a.getFilename).mkString("\n") + "\n DELETE ALL?"
            if (runUIwait(Dialog.showYesNo(msg)) == true) {
              tmp.map( f => sftp.rm(remoteBasePath + "/" + cp + "/" + f.getFilename) )
              sftp.rmdir(remoteBasePath + "/" + cp)
              return
            }
          }
          throw sftpe
        }
      }
    } else {
      sftp.rm(remoteBasePath + "/" + cp)
    }
  }
  def putfile(from: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(from)
    val rp = remoteBasePath + "/" + cp
    if (isdir)
      sftp.mkdir(rp)
    else
      sftp.put(localBasePath + "/" + cp, rp)
    sftp.setMtime(rp, (mtime/1000).toInt)
  }
  def getfile(from: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(from)
    val lp = localBasePath + "/" + cp
    if (isdir) {
      Files.createDirectory(Paths.get(lp))
    } else {
      sftp.get(remoteBasePath + "/" + cp, lp)
    }
    Files.setLastModifiedTime(Paths.get(lp), FileTime.fromMillis(mtime))
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

  def list(subfolder: String, filterregexp: String, receiver: ActorRef, recursive: Boolean, viaActor: Boolean) = {
    debug(s"listrecsftp(rbp=$remoteBasePath sf=$subfolder rec=$recursive) in thread ${Thread.currentThread().getId}")
    val reslist = new ArrayBuffer[VirtualFile]
    def VFfromLse(fullFilePath: String, lse: ChannelSftp#LsEntry) = {
      new VirtualFile {
        path= fullFilePath.substring(remoteBasePath.length)
        if (path == "") path = "/"
        modTime = lse.getAttrs.getMTime.toLong * 1000
        size = lse.getAttrs.getSize
        if (lse.getAttrs.isDir && path != "/") path += "/"
      }
    }
    def parseContent(folder: String) {
      debug("parseContent: " + folder)
      val xx = sftp.ls(folder)
      val tmp = new ListBuffer[ChannelSftp#LsEntry]
      for (obj <- xx ) { tmp += obj.asInstanceOf[ChannelSftp#LsEntry] } // doesn't work otherwise!
      val ord = new Ordering[ChannelSftp#LsEntry]() { def compare(l: ChannelSftp#LsEntry, r: ChannelSftp#LsEntry) = l.getFilename compare r.getFilename }
      for (obj <- tmp.sorted(ord) ) {
        if (stopRequested) return
        if (!obj.getFilename.equals(".") && !obj.getFilename.equals("..")) {
          val fullFilePath = folder + "/" + obj.getFilename
          val vf = VFfromLse(fullFilePath, obj)
          if ( !vf.fileName.matches(filterregexp) ) {
            if (viaActor) receiver ! addFile(vf, isLocal) else reslist += vf
            if (obj.getAttrs.isDir && recursive ) {
              parseContent(fullFilePath)
            }
          }
        }
      }
      unit()
    }
    debug("searching " + remoteBasePath + "/" + subfolder)
    val sp = remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder
    val sftpsp = sftpexists(sp)
    if (sftpsp != null) { // not nice, copied basically from above. but no other way
      val vf = VFfromLse(sp, sftpsp)
      if ( !vf.fileName.matches(filterregexp) ) {
        if (viaActor) receiver ! addFile(vf, isLocal) else reslist += vf
        if (sftpsp.getAttrs.isDir) {
          parseContent(sp)
        }
      }
    }
    debug("parsing done")
    if (viaActor) { receiver ! 'done ; null } else reslist
  }

  class MyUserInfo(val user: String, val password: String) extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    var getPassCount = 0
    def getPassword = {
      debug(s"getPassword passcount = $getPassCount")
      getPassCount += 1
      if (getPassCount < 2 && password != "") password
      else runUIwait(Dialog.showInputString("Enter password (to keep password: add to URI string, it will be encrypted):")).asInstanceOf[String]
    }
    def promptYesNo(str: String) : Boolean = {
      runUIwait(Dialog.showYesNo(str)) == true
    }

    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null

    def getPassphrase: String = ""

    def promptPassword(message: String): Boolean = { debug("prompt pwd") ; true }

    def promptPassphrase(message: String): Boolean = { debug("prompt pwd") ; true }

    def showMessage(message: String) { runUIwait(Dialog.showMessage(message)) }
  }

  class MyJschLogger extends jsch.Logger {

    def isEnabled(level: Int) = true

    def log(level: Int, message: String) {
      level match {
        case jsch.Logger.INFO => info("jsch: " + message)
        case jsch.Logger.WARN => warn("jsch: " + message)
        case jsch.Logger.ERROR | jsch.Logger.FATAL => error("jsch: " + message)
      }
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
  } else info("Can't get private key file, os not supported yet." )
  if (prvkeypath != "") {
    debug("prv key: " + prvkeypath)
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

  override def finish() {
    super.finish()
    if (sftp.isConnected) sftp.disconnect()
    if (session.isConnected) session.disconnect()
  }
}

abstract class GeneralConnection(isLocal: Boolean) extends Logging {
//  var conntype: ConnType.Value
  var localBasePath: String = ""
  var remoteBasePath: String = ""
  var filterregex: Regex = new Regex(""".*""")
  @volatile var stopRequested = false
  def getfile(from: String, mtime: Long)
  def putfile(from: String, mtime: Long)
  def deletefile(what: String, mtime: Long)
  def list(where: String, filterregexp: String, receiver: ActorRef, recursive: Boolean, viaActor: Boolean): ArrayBuffer[VirtualFile]
  def finish() {
    stopRequested = true
  }
  def checkIsDir(path: String) = {
    val isdir = path.endsWith("/")
    val resp = if (isdir) path.substring(0, path.length-1) else path
    (resp, isdir)
  }
}

// path below baspath with a leading "/"
// if ends on "/", is dir!
class VirtualFile(var path: String, var modTime: Long, var size: Long) extends Ordered[VirtualFile] {
  var tagged = false // for cachelist: tagged if local/remote existing, does not need to be added "cacheonly"

  def this() = this("",0,0)
//  def getPathString = if (path == "") "<root>" else path
  def fileName : String = if (path == "/") "/" else path.split("/").last
  override def toString: String = "["+path+"]:"+modTime+","+size

  def isDir = { path.endsWith("/") }

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[VirtualFile] && (this.hashCode() == that.asInstanceOf[VirtualFile].hashCode())
  }
  override def hashCode = {
    path.hashCode + modTime.hashCode + size.hashCode
  }

  def compare(that: VirtualFile): Int = path.compare(that.path)
}

