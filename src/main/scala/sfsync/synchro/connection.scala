package sfsync.synchro

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.file.attribute.FileTime

import com.jcraft.jsch
import com.jcraft.jsch.{ChannelSftp, SftpATTRS, SftpException, SftpProgressMonitor}
import sfsync.store.Tools
import sfsync.util.{Helpers, Logging}
import sfsync.util.Helpers._

import scala.Predef._
import scala.collection.JavaConversions._
import scala.collection.mutable._
import scala.util.matching.Regex

class cachedFile(path: String, modTime: Long, size: Long) {
}

class MyURI(var protocol: String, var username: String, var password: String, var host: String, var port: String) {
  val regexinet = new Regex("""(\S+)://(\S+)@(\S+):(\S+)""")
  def this() = this("","","","","")
  def parseString(s: String): Boolean = {
    s match {
      case "file:///" => protocol = "file"; true
      case regexinet(prot1, userinfo, host1, port1) =>
        protocol = prot1
        host = host1
        port = port1
        val uis = userinfo.split(":")
        uis.length match {
          case 1 => username = uis(0); password = ""
          case 2 => username = uis(0); password = uis(1)
        }
        true
      case _ => false
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

// path below baspath with a leading "/"
// if ends on "/", is dir!
class VirtualFile(var path: String, var modTime: Long, var size: Long) extends Ordered[VirtualFile] {
  // modtime in milliseconds since xxx
  var tagged = false // for cachelist: tagged if local/remote existing, does not need to be added "cacheonly"

  def this() = this("",0,0)
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

abstract class GeneralConnection(isLocal: Boolean, cantSetDate: Boolean) extends Logging {
  var localBasePath: String = ""
  var remoteBasePath: String = ""
  var filterregex: Regex = new Regex(""".*""")
  val debugslow = false
  def getfile(from: String, mtime: Long, to: String)
  def getfile(from: String, mtime: Long)
  def putfile(from: String, mtime: Long): Long // returns new mtime if cantSetDate
  def mkdirrec(absolutePath: String)
  def deletefile(what: String, mtime: Long)
  def list(subfolder: String, filterregexp: String, action: (VirtualFile) => Unit, recursive: Boolean)

  var onProgress = (progressVal: Double) => {}

  // return dir (most likely NOT absolute path but subfolder!) without trailing /
  def checkIsDir(path: String) = {
    val isdir = path.endsWith("/")
    val resp = if (isdir) path.substring(0, path.length-1) else path
    (resp, isdir)
  }
  def cleanUp() = {}
}

class LocalConnection(isLocal: Boolean, cantSetDate: Boolean) extends GeneralConnection(isLocal, cantSetDate) {

  def deletefile(what: String, mtime: Long) {
    val (cp, _) = checkIsDir(what)
    val fp = Paths.get(remoteBasePath + "/" + cp)
    try {
      Files.delete(fp)
    } catch {
      case ee: java.nio.file.DirectoryNotEmptyException =>
        val dir = Files.newDirectoryStream(fp).toList
        if (runUIwait(dialogOkCancel("Warning", s"Directory \n $cp \n not empty, DELETE ALL?", "Content:\n" + dir.map(a => a.toFile.getName).mkString("\n"))) == true) {
          dir.foreach(f => Files.delete(f) )
          Files.delete(fp)
          return
        }
    }
  }
  def putfile(from: String, mtime: Long): Long = {
    val (cp, isdir) = checkIsDir(from)
    debug(s"from=$from isdir=$isdir")
    if (isdir) { // ensure that target path exists
      val abspath = remoteBasePath + "/" + cp
      if (!Files.exists(Paths.get(abspath).getParent)) {
        debug(s"creating folder $cp")
        mkdirrec(Paths.get(abspath).getParent.toString)
      }
    }
    Files.copy(Paths.get(localBasePath + "/" + cp), Paths.get(remoteBasePath + "/" + cp), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
    mtime
  }
  def getfile(from: String, mtime: Long, to: String) {
    val (cp, isdir) = checkIsDir(from)
    if (isdir) { // ensure that target path exists
      Files.createDirectories(Paths.get(to)) // simply create parents if necessary, avoids separate check
    } else {
      Files.copy(Paths.get(remoteBasePath + "/" + cp), Paths.get(to), StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
    }
    Files.setLastModifiedTime(Paths.get(to), FileTime.fromMillis(mtime))
  }
  def getfile(from: String, mtime: Long) {
    val (cp, _) = checkIsDir(from)
    val lp = localBasePath + "/" + cp
    getfile(from, mtime, lp)
  }

  // include the subfolder but root "/" is not allowed!
  def list(subfolder: String, filterregexp: String, action: (VirtualFile) => Unit, recursive: Boolean) {
    debug(s"listrec(rbp=$remoteBasePath sf=$subfolder rec=$recursive) in thread ${Thread.currentThread().getId}")
    // scalax.io is horribly slow, there is an issue filed
    def parseContent(cc: Path, goDeeper: Boolean) {
      // on mac 10.8 with oracle java 7, filenames are encoded with strange 'decomposed unicode'. grr
      // this is in addition to the bug that LC_CTYPE is not set. grrr
      // don't use cc.getPath directly!!
      if (Helpers.failat == 4) throw new UnsupportedOperationException("fail 4")
      val javaPath = toJavaPathSeparator(cc.toString)
      val fixedPath = java.text.Normalizer.normalize(javaPath, java.text.Normalizer.Form.NFC)
      var strippedPath: String = if (fixedPath == remoteBasePath) "/" else fixedPath.substring(remoteBasePath.length)
      if (Files.isDirectory(cc) && strippedPath != "/") strippedPath += "/"
      val vf = new VirtualFile(strippedPath, Files.getLastModifiedTime(cc).toMillis, Files.size(cc))
      if ( !vf.fileName.matches(filterregexp)) {
        if (debugslow) Thread.sleep(100)
        action(vf)
        if (Files.isDirectory(cc) && goDeeper ) {
          val dir = Files.newDirectoryStream(cc)
          for (cc1 <- dir) parseContent(cc1, goDeeper = recursive)
          dir.close()
        }
      }
      unit()
    }
    val sp = Paths.get(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
    if (Files.exists(sp)) {
      parseContent(sp, goDeeper = true)
    }
    debug(s"listrec DONE (rbp=$remoteBasePath sf=$subfolder rec=$recursive) in thread ${Thread.currentThread().getId}")
  }

  def mkdirrec(absolutePath: String) = {
    Files.createDirectories(Paths.get(absolutePath))
  }
}


class SftpConnection(isLocal: Boolean, cantSetDate: Boolean, var uri: MyURI) extends GeneralConnection(isLocal, cantSetDate) {

  class MySftpProgressMonitor extends SftpProgressMonitor {
    var bytesTransferred: Long = 0
    var bytesTotal: Long = 0

    def init(op: Int, src: String, dest: String, max: Long) {
      bytesTransferred = 0
      bytesTotal = max
    }

    def count(bytes: Long): Boolean = {
      bytesTransferred += bytes
      onProgress(bytesTransferred.toDouble/bytesTotal)
      true //!stopRequested // abort transfer if requested
    }
    def end() = {}
  }
  def deletefile(what: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(what)
    if (isdir) {
      try {
        sftp.rmdir(remoteBasePath + "/" + cp)
      } catch {
        case sftpe: SftpException => // unfortunately only "Failure" ; checking for content would be slow
          val xx = sftp.ls(remoteBasePath + "/" + cp)
          if (xx.nonEmpty) {
            val tmp = new ListBuffer[ChannelSftp#LsEntry]
            for (obj <- xx ) {
              val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
              lse.getFilename match {
                case "." | ".." =>
                case s => tmp += lse
              }
            }
            if (runUIwait(dialogOkCancel("Warning", s"Directory \n $cp \n not empty, DELETE ALL?", "Content:\n" + tmp.map(a => a.getFilename).mkString("\n"))) == true) {
              tmp.foreach(f => sftp.rm(remoteBasePath + "/" + cp + "/" + f.getFilename) )
              sftp.rmdir(remoteBasePath + "/" + cp)
              return
            }
          }
          throw sftpe
      }
    } else {
      sftp.rm(remoteBasePath + "/" + cp)
    }
  }
  def putfile(from: String, mtime: Long): Long = {
    val (cp, isdir) = checkIsDir(from)
    val rp = remoteBasePath + "/" + cp
    if (isdir) {
      def checkit(p: String) { // recursively create parents
        val parent = Paths.get(p).getParent.toString
        if (sftpexists(parent) == null) {
          checkit(parent)
          sftp.mkdir(parent)
        }
      }
      checkit(rp)
      sftp.mkdir(rp)
      mtime // dirs don't need mtime
    } else {
      sftp.put(localBasePath + "/" + cp, rp, progressSftp)
      if (progressSftp.bytesTotal != progressSftp.bytesTransferred) { // interrupted!
        sftp.rm(rp) // delete partially transferred files
        -1
      } else {
        if (cantSetDate) {
          sftp.lstat(rp).getMTime.toLong * 1000
        } else {
          sftp.setMtime(rp, (mtime/1000).toInt)
          mtime
        }
      }
    }
  }
  def getfile(from: String, mtime: Long, to: String) {
    val (cp, isdir) = checkIsDir(from)
    if (isdir) {
      Files.createDirectories(Paths.get(to)) // simply create parents if necessary, avoids separate check
      Files.setLastModifiedTime(Paths.get(to), FileTime.fromMillis(mtime))
    } else {
      sftp.get(remoteBasePath + "/" + cp, to, progressSftp)
      if (progressSftp.bytesTotal != progressSftp.bytesTransferred) { // interrupted!
        Files.delete(Paths.get(to)) // delete partially transferred files
      } else {
        Files.setLastModifiedTime(Paths.get(to), FileTime.fromMillis(mtime))
      }
    }
  }
  def getfile(from: String, mtime: Long) {
    val (cp, _) = checkIsDir(from)
    val lp = localBasePath + "/" + cp
    getfile(from, mtime, lp)
  }
  def sftpexists(sp: String): SftpATTRS = {
    var resls: SftpATTRS = null
    try {
      resls = sftp.stat(sp) // throws exception if not
    } catch {
      case e: SftpException if e.id == ChannelSftp.SSH_FX_NO_SUCH_FILE => debug(e)
      case e: Throwable => throw e
    }
    resls
  }

  def list(subfolder: String, filterregexp: String, action: (VirtualFile) => Unit, recursive: Boolean) {
    debug(s"listrecsftp(rbp=$remoteBasePath sf=$subfolder rec=$recursive) in thread ${Thread.currentThread().getId}")
    def VFfromSftp(fullFilePath: String, attrs: SftpATTRS) = {
      new VirtualFile {
        path= fullFilePath.substring(remoteBasePath.length)
        if (path == "") path = "/"
        modTime = attrs.getMTime.toLong * 1000
        size = attrs.getSize
        if (attrs.isDir && path != "/") path += "/"
      }
    }
    def parseContent(folder: String) {
      if (Helpers.failat == 3) throw new UnsupportedOperationException("fail 3")
      val xx = sftp.ls(folder)
      val tmp = new ListBuffer[ChannelSftp#LsEntry]
      for (obj <- xx ) { tmp += obj.asInstanceOf[ChannelSftp#LsEntry] } // doesn't work otherwise!
      val ord = new Ordering[ChannelSftp#LsEntry]() { def compare(l: ChannelSftp#LsEntry, r: ChannelSftp#LsEntry) = l.getFilename compare r.getFilename }
      for (obj <- tmp.sorted(ord) ) {
        // if (stopRequested) return
        if (!obj.getFilename.equals(".") && !obj.getFilename.equals("..")) {
          val fullFilePath = folder + "/" + obj.getFilename
          val vf = VFfromSftp(fullFilePath, obj.getAttrs)
          if ( !vf.fileName.matches(filterregexp) ) {
            action(vf)
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
    if (sftpsp != null) { // not nice: duplicate code (above)
      val vf = VFfromSftp(sp, sftpsp) // not nice: duplicate code (above)
      if ( !vf.fileName.matches(filterregexp) ) {
        action(vf)
        if (sftpsp.isDir) {
          parseContent(sp)
        }
      }
    }
    debug("parsing done")
  }

  class MyUserInfo(val user: String, val password: String) extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    var getPassCount = 0
    def getPassword = {
      debug(s"getPassword passcount = $getPassCount")
      getPassCount += 1
      val pwd = if (getPassCount < 2 && password != "")
        password
      else
        runUIwait(dialogInputString("SSH", "SSH password required. To store password: add to URI string, it will be encrypted", "Password:")).asInstanceOf[String]
      if (pwd == "")
        throw new Exception("Sftp login aborted.")
      pwd
    }
    def promptYesNo(str: String) : Boolean = {
      runUIwait(dialogOkCancel("SSH", "SSH subsystem question:", str)) == true
    }

    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null

    def getPassphrase: String = ""

    def promptPassword(message: String): Boolean = { debug("prompt pwd") ; true }

    def promptPassphrase(message: String): Boolean = { debug("prompt pwd") ; true }

    def showMessage(message: String) {
      debug("SSH message: " + message)
      //runUIwait(Dialog.showMessage(message))
    }
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
  var progressSftp = new MySftpProgressMonitor

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
      password = Tools.crypto.decrypt(password.substring(2))
    }
  }
  var session = jSch.getSession(uri.username, uri.host, uri.port.toInt)

  var ui = new MyUserInfo(uri.username, password)
  session.setUserInfo(ui)
//  session.setConfig("kex", "diffie-hellman-group1-sha1") // for cm13 testing... no way
  session.connect()
  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]
  sftp.connect(5000)
  if (!sftp.isConnected) {
    throw new Exception("sftp not connected!")
  }
  if (Helpers.failat == 2) throw new UnsupportedOperationException("fail 2")

  override def cleanUp() {
    super.cleanUp()
    if (sftp.isConnected) sftp.disconnect()
    if (session.isConnected) session.disconnect()
  }

  def mkdirrec(absolutePath: String) {
    throw new NotImplementedError("mkdirrec for sftp")
  }
}


