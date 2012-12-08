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

class LocalConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    Path.fromString(remoteBasePath + "/" + what.path).delete(force = true)
    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) {
    Path.fromString(localBasePath + "/" + from.path) copyTo Path.fromString(remoteBasePath + "/" + to.path)
  }
  def getfile(from: VirtualFile, to: VirtualFile) {
    Path.fromString(remoteBasePath + "/" + from.path) copyTo Path.fromString(localBasePath + "/" + to.path)
  }
  def listrec(subfolder: String, receiver: Actor) = {
    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: Path) : Unit = {
      println("parsing " + folder)
      folder.children().foreach(cc => {
        val vf = new VirtualFile {
          path=cc.path.substring(remoteBasePath.length + 1) // without leading '/'
          modTime = cc.lastModified
          size = cc.size.get
          isDir = if (cc.isDirectory) 1 else 0
        }
        println("got " + vf)
        list += vf
        if (receiver != null) receiver ! vf
        if (cc.isDirectory) {
          parseContent(cc)
        }
      })
    }
    parseContent(Path.fromString(remoteBasePath + "/" + subfolder))
    if (receiver != null) receiver ! 'done
    list
  }
  def finish() {}
}

class SftpConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    sftp.rm(remoteBasePath + "/" + what.path)
    println("deleted " + remoteBasePath + "/" + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) {
    sftp.put(localBasePath + "/" + from.path, remoteBasePath + "/" + to.path) // TODO: progressmonitor!
  }
  def getfile(from: VirtualFile, to: VirtualFile) {
    sftp.put(remoteBasePath + "/" + from.path, localBasePath + "/" + to.path) // TODO: progressmonitor!
  }

  def listrec(subfolder: String, receiver: Actor) = {
    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: String) : Unit = {
      val xx = sftp.ls(folder)
      println("parsing " + folder + " : size=" + xx.size())
      for (obj <- xx) {
        val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
        println("parsing " + lse.getFilename)
        if (!lse.getFilename.equals(".") && !lse.getFilename.equals("..")) {
          val fullFilePath = folder + "/" + lse.getFilename
          val vf = new VirtualFile {
            path=(fullFilePath).substring(remoteBasePath.length + 2) // without leading '/'
//            printf("times=" + lse.getAttrs.getMTime + " " + lse.getAttrs.getMtimeString + " " + lse.getAttrs.getATime)
            modTime = lse.getAttrs.getMTime.toLong * 1000
            size = lse.getAttrs.getSize
            isDir = if (lse.getAttrs.isDir) 1 else 0
          }
          println("got " + vf)
          list += vf
          if (receiver != null) receiver ! vf
          if (lse.getAttrs.isDir) {
            parseContent(fullFilePath)
          }
        }
      }
    }
    parseContent(remoteBasePath + "/" + subfolder)
    if (receiver != null) receiver ! 'done
    list
  }

  import scala.collection.immutable.Map

  class MyUserInfo extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    def getPassword : String = {
      new Dialog("Enter password:").showInputString
    }
    def promptYesNo(str: String) : Boolean = {
      new Dialog(str).showYesNo
    }

    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null

    def getPassphrase: String = ""

    def promptPassword(message: String): Boolean = { println("prompt pwd") ; true }

    def promptPassphrase(message: String): Boolean = { println("prompt pwd") ; true }

    def showMessage(message: String) { println("message: " + message) }
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

  val prvkey: Array[Byte] = scalax.file.Path.fromString("/Users/wolle/.ssh/id_dsa").bytes.toArray
    jSch.addIdentity("wolle",prvkey,null,Array[Byte]())
    var session = jSch.getSession("wolle", "localhost", 22)
//  jSch.addIdentity("loeffler",prvkey,null,Array[Byte]())
//  var session = jSch.getSession("loeffler", "data01.physics.leidenuniv.nl", 22)

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
  def getfile(from: VirtualFile, to: VirtualFile)
  def putfile(from: VirtualFile, to: VirtualFile)
  def deletefile(what: VirtualFile)
  def listrec(where: String, receiver: Actor): ListBuffer[VirtualFile]
  def finish()
}

class VirtualFile extends Ordered[VirtualFile] {
  var path: String = "" // TODO: is this below basepath or not? YES!!!!!!!
  var modTime: Long = 0
  var size: Long = 0
  var isDir: Int = 0
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

