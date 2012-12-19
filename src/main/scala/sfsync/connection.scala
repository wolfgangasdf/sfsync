package sfsync.synchro

import sfsync.Main.Dialog
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.Predef._
import scala.collection.JavaConversions._
import actors.Actor
import scalax.file.Path
import com.jcraft.jsch
import jsch.ChannelSftp

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
  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
//    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: Path) : Unit = {
//      println("parsing " + folder)
      for (cc <- folder.children().toList.sorted) { // sorted improves performance a lot of course
        val vf = new VirtualFile {
          path=cc.path.substring(remoteBasePath.length + 1) // without leading '/'
          modTime = cc.lastModified
          size = cc.size.get
          isDir = if (cc.isDirectory) 1 else 0
        }
        if ( !vf.fileName.matches(filterregexp) ) {
//          println("got " + vf)
          list += vf
          if (receiver != null) receiver ! vf
          if (cc.isDirectory) {
            parseContent(cc)
          }
        }
      }
    }
    val sp = Path.fromString(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
    println("sp=" + sp)
    if (sp.exists) parseContent(sp)
    if (receiver != null) receiver ! 'done
    list
  }
  def finish() {}
}

class SftpConnection(var uri: java.net.URI) extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    sftp.rm(remoteBasePath + "/" + what.path)
//    println("deleted " + remoteBasePath + "/" + what.path)
  }
  def putfile(from: VirtualFile) {
    val rp = remoteBasePath + "/" + from.path
    sftp.put(localBasePath + "/" + from.path, rp) // TODO: progressmonitor!
    sftp.setMtime(rp, (from.modTime/1000).toInt)
  }
  def getfile(from: VirtualFile) {
    val lp = localBasePath + "/" + from.path
    sftp.get(remoteBasePath + "/" + from.path, lp) // TODO: progressmonitor!
    Path.fromString(lp).lastModified = from.modTime
  }

  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: String) : Unit = {
      val xx = sftp.ls(folder)
//      println("parsing " + folder + " : size=" + xx.length)
      val tmp = new ListBuffer[ChannelSftp#LsEntry]
      for (obj <- xx ) { tmp += obj.asInstanceOf[ChannelSftp#LsEntry] } // doesn't work otherwise!
      val ord = new Ordering[ChannelSftp#LsEntry]() { def compare(l: ChannelSftp#LsEntry, r: ChannelSftp#LsEntry) = l.getFilename compare r.getFilename }
      for (obj <- tmp.sorted(ord) ) {
        val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
//        println("lse=" + lse.getFilename)
        if (!lse.getFilename.equals(".") && !lse.getFilename.equals("..")) {
          val fullFilePath = folder + "/" + lse.getFilename
          val vf = new VirtualFile {
//            println("ffp=" + fullFilePath)
            path=(fullFilePath).substring(remoteBasePath.length + 1) // without leading '/'
//            printf("times=" + lse.getAttrs.getMTime + " " + lse.getAttrs.getMtimeString + " " + lse.getAttrs.getATime)
            modTime = lse.getAttrs.getMTime.toLong * 1000
            size = lse.getAttrs.getSize
            isDir = if (lse.getAttrs.isDir) 1 else 0
          }
          if ( !vf.fileName.matches(filterregexp) ) {
//            println("got " + vf)
            list += vf
            if (receiver != null) receiver ! vf
            if (lse.getAttrs.isDir) {
              parseContent(fullFilePath)
            }
          }
        }
      }
    }
    parseContent(remoteBasePath + (if (subfolder.length>0) "/" else "") + subfolder)
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
  def listrec(where: String, filterregexp: String, receiver: Actor): ListBuffer[VirtualFile]
  def finish()
}

class VirtualFile extends Ordered[VirtualFile] {
  var path: String = "" // TODO: is this below basepath or not? YES!!!!!!!
  var modTime: Long = 0
  var size: Long = 0
  var isDir: Int = 0

  def fileName : String = { path.split("/").last }
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

