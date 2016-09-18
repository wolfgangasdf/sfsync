package sfsync.synchro

import java.io.IOException
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.file.attribute.FileTime
import java.util.concurrent.atomic.AtomicBoolean

import net.schmizz.sshj.common.StreamCopier
import net.schmizz.sshj.common.StreamCopier.Listener
import net.schmizz.sshj.sftp.Response.StatusCode
import net.schmizz.sshj.sftp.{FileAttributes, FileMode, RemoteResourceInfo, SFTPException}
import net.schmizz.sshj.xfer.TransferListener
import net.schmizz.sshj.SSHClient
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
  val interrupted = new AtomicBoolean(false)
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

  class MyTransferListener(var relPath: String = "") extends TransferListener {
    var bytesTransferred: Long = 0
    var bytesTotal: Long = 0

    override def directory(name: String): TransferListener = {
      new MyTransferListener(relPath + name + "/")
    }

    override def file(name: String, size: Long): Listener = {
      val path: String = relPath + name
      bytesTotal = size
      bytesTransferred = 0
      new StreamCopier.Listener() {
        def reportProgress(transferred: Long) {
          bytesTransferred = transferred
          if (interrupted.get) throw new InterruptedException("sftp connection interrupted")
          onProgress(bytesTransferred.toDouble/bytesTotal)
        }
      }
    }
  }

  def isDirectoryx(fa: FileAttributes) = {
    (fa.getType.toMask & FileMode.Type.DIRECTORY.toMask) > 0
  }

  var transferListener: MyTransferListener = _

  def deletefile(what: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(what)
    if (isdir) {
      try {
        sftpc.rmdir(remoteBasePath + "/" + cp)
      } catch {
        case ioe: IOException => // unfortunately only "Failure" ; checking for content would be slow
          val xx = sftpc.ls(remoteBasePath + "/" + cp)
          if (xx.nonEmpty) { // TODO test this well!
            val tmp = new ListBuffer[RemoteResourceInfo]
            for (obj <- xx ) {
              val lse = obj.asInstanceOf[RemoteResourceInfo]
              lse.getName match {
                case "." | ".." =>
                case s => tmp += lse
              }
            }
            if (runUIwait(dialogOkCancel("Warning", s"Directory \n $cp \n not empty, DELETE ALL?", "Content:\n" + tmp.map(a => a.getName).mkString("\n"))) == true) {
              tmp.foreach(f => sftpc.rm(remoteBasePath + "/" + cp + "/" + f.getName) )
              sftpc.rmdir(remoteBasePath + "/" + cp)
              return
            }
          }
      }
    } else {
      sftpc.rm(remoteBasePath + "/" + cp)
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
          sftpc.mkdir(parent)
        }
      }
      checkit(rp)
      sftpc.mkdir(rp)
      mtime // dirs don't need mtime
    } else {
      try {
        sftpt.upload(localBasePath + "/" + cp, rp)
      } catch {
        case e: Exception =>
          debug(s"putfile: exception: $e")
          if (transferListener.bytesTransferred > 0) { // file may be corrupted, but don't delete if nothing transferred
            // prevent delete of root-owned files if user in group admin, sftp rm seems to "override permissions"
            sftpc.rm(rp)
          }
          throw e
      }
      if (transferListener.bytesTotal != transferListener.bytesTransferred)
        throw new IllegalStateException(s"filesize mismatch: ${transferListener.bytesTotal} <> ${transferListener.bytesTransferred}")
      if (cantSetDate) {
        sftpc.mtime(rp) * 1000
      } else {
        mtime
      }
    }
  }
  def getfile(from: String, mtime: Long, to: String) {
    val (cp, isdir) = checkIsDir(from)
    if (isdir) {
      Files.createDirectories(Paths.get(to)) // simply create parents if necessary, avoids separate check
      Files.setLastModifiedTime(Paths.get(to), FileTime.fromMillis(mtime))
    } else {
      try {
        sftpt.download(remoteBasePath + "/" + cp, to)
      } catch {
        case e: Exception =>
          debug("getfile: exception " + e)
          if (transferListener.bytesTransferred > 0) // file may be corrupted, but don't delete if nothing transferred
            Files.delete(Paths.get(to)) // file may be corrupted

          throw e
      }
      if (transferListener.bytesTotal != transferListener.bytesTransferred)
        throw new IllegalStateException(s"filesize mismatch: ${transferListener.bytesTotal} <> ${transferListener.bytesTransferred}")

      Files.setLastModifiedTime(Paths.get(to), FileTime.fromMillis(mtime))
    }
  }
  def getfile(from: String, mtime: Long) {
    val (cp, _) = checkIsDir(from)
    val lp = localBasePath + "/" + cp
    getfile(from, mtime, lp)
  }
  def sftpexists(sp: String): FileAttributes = {
    var resls: FileAttributes = null
    try {
      resls = sftpc.stat(sp) // throws exception if not
    } catch {
      case e: SFTPException if e.getStatusCode == StatusCode.NO_SUCH_FILE => debug(e)
      case e: Throwable => throw e
    }
    resls
  }

  def list(subfolder: String, filterregexp: String, action: (VirtualFile) => Unit, recursive: Boolean) {
    debug(s"listrecsftp(rbp=$remoteBasePath sf=$subfolder rec=$recursive) in thread ${Thread.currentThread().getId}")

    def VFfromSftp(fullFilePath: String, attrs: FileAttributes) = {
      new VirtualFile {
        path= fullFilePath.substring(remoteBasePath.length)
        modTime = attrs.getMtime * 1000
        size = attrs.getSize
        if (isDirectoryx(attrs) && path != "/") path += "/"
      }
    }
    def parseContent(folder: String) {
      if (Helpers.failat == 3) throw new UnsupportedOperationException("fail 3")
      val rris = sftpc.ls(folder)
      val ord = new Ordering[RemoteResourceInfo]() { def compare(l: RemoteResourceInfo, r: RemoteResourceInfo) = l.getName compare r.getName }
      for (rri <- rris.sorted(ord) ) {
        // if (stopRequested) return
        if (!rri.getName.equals(".") && !rri.getName.equals("..")) {
          val vf = VFfromSftp(rri.getPath, rri.getAttributes)
          if ( !vf.fileName.matches(filterregexp) ) {
            action(vf)
            if (isDirectoryx(rri.getAttributes) && recursive ) {
              parseContent(rri.getPath)
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
        if (isDirectoryx(sftpsp)) {
          parseContent(sp)
        }
      }
    }
    debug("parsing done")
  }

//  class MyUserInfo(val user: String, val password: String) extends jsch.UserInfo with jsch.UIKeyboardInteractive {
//    var getPassCount = 0
//    def getPassword = {
//      debug(s"getPassword passcount = $getPassCount")
//      getPassCount += 1
//      val pwd = if (getPassCount < 2 && password != "")
//        password
//      else
//        runUIwait(dialogInputString("SSH", "SSH password required. To store password: add to URI string, it will be encrypted", "Password:")).asInstanceOf[String]
//      if (pwd == "")
//        throw new Exception("Sftp login aborted.")
//      pwd
//    }
//    def promptYesNo(str: String) : Boolean = {
//      runUIwait(dialogOkCancel("SSH", "SSH subsystem question:", str)) == true
//    }
//
//    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null
//
//    def getPassphrase: String = ""
//
//    def promptPassword(message: String): Boolean = { debug("prompt pwd") ; true }
//
//    def promptPassphrase(message: String): Boolean = { debug("prompt pwd") ; true }
//
//    def showMessage(message: String) {
//      debug("SSH message: " + message)
//      //runUIwait(Dialog.showMessage(message))
//    }
//  }
//

  // init

  System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "WARN")

  val ssh = new SSHClient()
  ssh.loadKnownHosts()

  var prvkeypath = ""

  ssh.connect(uri.host, uri.port.toInt)

  var password = uri.password
  if (password != "") {
    if (password.startsWith("##")) { // decode password
      password = Tools.crypto.decrypt(password.substring(2))
    }
    ssh.authPassword(uri.username, password)
  } else {
    ssh.authPublickey(uri.username)
  }
  val sftpc = ssh.newSFTPClient
  val sftpt = sftpc.getFileTransfer

  transferListener = new MyTransferListener()
  sftpt.setTransferListener(transferListener)

  if (Helpers.failat == 2) throw new UnsupportedOperationException("fail 2")

  override def cleanUp() {
    super.cleanUp()
    sftpc.close()
    if (ssh.isConnected) ssh.disconnect()
  }

  def mkdirrec(absolutePath: String) {
    throw new NotImplementedError("mkdirrec for sftp")
  }
}


