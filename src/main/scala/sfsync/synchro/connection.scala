package sfsync.synchro

import java.io.IOException
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.security.PublicKey
import java.util.concurrent.atomic.AtomicBoolean

import net.schmizz.sshj.SSHClient
import net.schmizz.sshj.common.StreamCopier.Listener
import net.schmizz.sshj.common.{KeyType, SecurityUtils}
import net.schmizz.sshj.sftp.Response.StatusCode
import net.schmizz.sshj.sftp.{FileAttributes, FileMode, RemoteResourceInfo, SFTPException}
import net.schmizz.sshj.transport.verification.OpenSSHKnownHosts
import net.schmizz.sshj.transport.verification.OpenSSHKnownHosts.HostEntry
import net.schmizz.sshj.userauth.UserAuthException
import net.schmizz.sshj.xfer.{FilePermission, FileSystemFile, TransferListener}
import sfsync.store.{DBSettings, Protocol, Tools}
import sfsync.util.Helpers._
import sfsync.util.{Helpers, Logging}

import scala.collection.JavaConverters._
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
  def toURIString: String = {
    protocol + "://" + username + ":" + password + "@" + host + ":" + port
  }
  override def toString: String = {
    s"$protocol,$username,$host,$port"
  }
}
object MyURI {
  def apply(s: String): MyURI = {
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

  def isDir: Boolean = { path.endsWith("/") }

  override def equals(that: Any): Boolean = {
    that.isInstanceOf[VirtualFile] && (this.hashCode() == that.asInstanceOf[VirtualFile].hashCode())
  }
  override def hashCode: Int = {
    path.hashCode + modTime.hashCode + size.hashCode
  }

  def compare(that: VirtualFile): Int = path.compare(that.path)
}

abstract class GeneralConnection(protocol: Protocol, isLocal: Boolean) extends Logging {
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

  //noinspection ScalaUnusedSymbol
  var onProgress: (Double, Double) => Unit = (progressVal: Double, bytePerSecond: Double) => {}

  // return dir (most likely NOT absolute path but subfolder!) without trailing /
  def checkIsDir(path: String): (String, Boolean) = {
    val isdir = path.endsWith("/")
    val resp = if (isdir) path.substring(0, path.length-1) else path
    (resp, isdir)
  }
  def cleanUp(): Unit = {}
}

class LocalConnection(protocol: Protocol, isLocal: Boolean) extends GeneralConnection(protocol, isLocal) {

  def deletefile(what: String, mtime: Long) {
    val (cp, _) = checkIsDir(what)
    val fp = Paths.get(remoteBasePath + "/" + cp)
    try {
      Files.delete(fp)
    } catch {
      case _: java.nio.file.DirectoryNotEmptyException =>
        val dir = Files.newDirectoryStream(fp).asScala.toList
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
        if (debugslow) Thread.sleep(500)
        action(vf)
        if (Files.isDirectory(cc) && goDeeper ) {
          val dir = Files.newDirectoryStream(cc)
          for (cc1 <- dir.asScala) parseContent(cc1, goDeeper = recursive)
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

  def mkdirrec(absolutePath: String): Unit = {
    Files.createDirectories(Paths.get(absolutePath))
  }
}


class SftpConnection(protocol: Protocol, isLocal: Boolean, var uri: MyURI) extends GeneralConnection(protocol, isLocal) {

  class MyTransferListener(var relPath: String = "") extends TransferListener {
    var bytesTransferred: Long = 0
    var lastBytesTransferred: Long = 0
    var bytesTotal: Long = 0
    var lastTime: Long = 0

    override def directory(name: String): TransferListener = {
      new MyTransferListener(relPath + name + "/")
    }

    override def file(name: String, size: Long): Listener = {
      bytesTotal = size
      bytesTransferred = 0
      lastBytesTransferred = 0
      lastTime = System.nanoTime
      (transferred: Long) => {
        bytesTransferred = transferred
        if (interrupted.get) throw new InterruptedException("sftp connection interrupted")
        val tnow = System.nanoTime
        if ((tnow - lastTime) / 1.0e9 > 0.5) {
          val byps = (bytesTransferred - lastBytesTransferred) / ((tnow - lastTime) / 1.0e9)
          lastTime = tnow
          lastBytesTransferred = bytesTransferred
          onProgress(bytesTransferred.toDouble / bytesTotal, byps)
        }
      }
    }
  }

  def isDirectoryx(fa: FileAttributes): Boolean = {
    (fa.getType.toMask & FileMode.Type.DIRECTORY.toMask) > 0
  }

  var transferListener: MyTransferListener = _

  def deletefile(what: String, mtime: Long) {
    val (cp, isdir) = checkIsDir(what)
    if (isdir) {
      try {
        sftpc.rmdir(remoteBasePath + "/" + cp)
      } catch {
        case _: IOException => // unfortunately only "Failure" ; checking for content would be slow
          val xx = sftpc.ls(remoteBasePath + "/" + cp).asScala
          if (xx.nonEmpty) {
            val tmp = new ListBuffer[RemoteResourceInfo]
            for (obj <- xx ) {
              val lse = obj.asInstanceOf[RemoteResourceInfo]
              lse.getName match {
                case "." | ".." =>
                case _ => tmp += lse
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

    def setAttr(changeperms: Boolean): Unit = {
      val lf = new FileSystemFile(localBasePath + "/" + cp)
      val fab = new FileAttributes.Builder
      if (changeperms) {
        val perms = FilePermission.fromMask(lf.getPermissions)
        if (protocol.remGroupWrite.value) perms.add(FilePermission.GRP_W) else perms.remove(FilePermission.GRP_W)
        if (protocol.remOthersWrite.value) perms.add(FilePermission.OTH_W) else perms.remove(FilePermission.OTH_W)
        fab.withPermissions(perms)
      }
      fab.withAtimeMtime(lf.getLastAccessTime, lf.getLastModifiedTime)
      sftpc.setattr(rp, fab.build())
    }

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
      if (protocol.doSetPermissions.value) setAttr(true)
      mtime // dirs don't need mtime
    } else {
      try {
        if (!Files.isReadable(Paths.get(localBasePath + "/" + cp))) throw new IllegalStateException("can't read file " + cp)
        sftpt.upload(localBasePath + "/" + cp, rp) // use this in place of sftpc.put to not always set file attrs
        if (protocol.doSetPermissions.value) setAttr(true)
        else if (!protocol.cantSetDate.value) setAttr(false)
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
      if (protocol.cantSetDate.value) {
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
        // sftpt.download erases local file if it exists also if remote file can't be read
        val tmpf = createTempFile("sfsync-tempfile", ".dat")
        sftpt.download(remoteBasePath + "/" + cp, tmpf.getAbsolutePath)
        Files.move(tmpf.toPath, Paths.get(to), StandardCopyOption.REPLACE_EXISTING)
      } catch {
        case e: Exception =>
          debug("getfile: exception " + e)
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
      val rris = sftpc.ls(folder).asScala
      val ord = new Ordering[RemoteResourceInfo]() { def compare(l: RemoteResourceInfo, r: RemoteResourceInfo): Int = l.getName compare r.getName }
      for (rri <- rris.sorted(ord)) {
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

  // init

  System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "WARN")

  // see ConsoleKnownHostsVerifier
  class MyHostKeyVerifier extends OpenSSHKnownHosts(DBSettings.knownHostsFile) {
    override def hostKeyUnverifiableAction(hostname: String, key: PublicKey): Boolean = {
      if (runUIwait(dialogOkCancel("SFTP server verification", s"Can't verify public key of server $hostname",
        s"Fingerprint:\n${SecurityUtils.getFingerprint(key)}\nPress OK to connect and add to SFSync's known_hosts.")).asInstanceOf[Boolean]) {
        entries.add(new OpenSSHKnownHosts.SimpleEntry(null, hostname, KeyType.fromKey(key), key))
        write()
        true
      } else false
    }

    override def hostKeyChangedAction(entry: HostEntry, hostname: String, key: PublicKey): Boolean = {
      if (runUIwait(dialogOkCancel("SFTP server verification", s"Host key of server $hostname has changed!",
        s"Fingerprint:\n${SecurityUtils.getFingerprint(key)}\nPress OK if you are 100% sure if this change was intended.")).asInstanceOf[Boolean]) {
        entries.remove(entry)
        entries.add(new OpenSSHKnownHosts.SimpleEntry(null, hostname, KeyType.fromKey(key), key))
        write()
        true
      } else false
    }
  }

  val ssh = new SSHClient()
  ssh.addHostKeyVerifier(new MyHostKeyVerifier)
  ssh.connect(uri.host, uri.port.toInt)

  private var password = uri.password
  if (password.startsWith("##"))
    password = Tools.crypto.decrypt(password.substring(2)) // decode password

  try {
    ssh.authPublickey(uri.username)
  } catch {
    case e: UserAuthException =>
      info("Public key auth failed: " + e)
      info("auth methods: " + ssh.getUserAuth.getAllowedMethods.asScala.mkString(","))
      // under win7 this doesn't work, try password in any case
//      if (ssh.getUserAuth.getAllowedMethods.exists(s => s == "keyboard-interactive" || s == "password" )) {
        if (password == "") {
          val res = runUIwait(dialogInputString("SSH", s"Public key auth failed, require password. \nNote: to store the password: add to URI string, it will be encrypted", "Password:")).asInstanceOf[String]
          if (res != "") password = res
        }
        if (password != "") {
          info("Trying password login...")
          ssh.authPassword(uri.username, password)
        } else throw new UserAuthException("No password")
//      }
  }
  if (!ssh.isAuthenticated) {
    throw new UserAuthException("Not authenticated!")
  } else info("Authenticated!")


  private val sftpc = ssh.newSFTPClient
  private val sftpt = sftpc.getFileTransfer

  transferListener = new MyTransferListener()
  sftpt.setTransferListener(transferListener)

  sftpt.setPreserveAttributes(false) // don't set permissions remote! Either by user or not at all.

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


