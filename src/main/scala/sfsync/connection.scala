package sfsync.synchro

import scala.util.matching.Regex
import scala.collection.mutable._
import scala.collection.immutable._
import collection.mutable
import actors.Actor
import actors.Actor._
import scalax.io._
import scalax.file.Path
import scalax.file.PathMatcher.IsFile
import com.jcraft.jsch
import scala.Predef._
import javax.swing.JOptionPane
import scala.collection.JavaConversions._
import sfsync.Main.Dialog

class cachedFile(path: String, modTime: Long, size: Long) {

}


case class getfile(from: VirtualFile, to: VirtualFile)
case class putfile(from: VirtualFile, to: VirtualFile)
case class deletefile(what: VirtualFile)
case class listrec(where: String, receiver: Actor)


class LocalConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    Path.fromString(remoteBasePath + what.path).delete(force = true)
    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) {
    Path.fromString(localBasePath + from.path) copyTo Path.fromString(remoteBasePath + to.path)
  }
  def getfile(from: VirtualFile, to: VirtualFile) {
    Path.fromString(remoteBasePath + from.path) copyTo Path.fromString(localBasePath + to.path)
  }  // the main actor
  def listrec(subfolder: String, receiver: Actor) = {
    var list = new ListBuffer[VirtualFile]()
    println("searching " + remoteBasePath + subfolder)
    // TODO change this to manual search since could be slow smb mount!
    // or is this automatically lazy???
//    val res: Option[Path] = Path.fromString(remoteBasePath + subfolder).descendants().find(_.name == ".gitignore")
    Path.fromString(remoteBasePath + subfolder) ** "*" foreach { ppp =>
      val vf = new VirtualFile {
        path=ppp.path.substring(remoteBasePath.length + 2) // without leading '/'
        modTime = ppp.lastModified
        size = ppp.size.get
        isDir = if (ppp.isDirectory) 1 else 0
      }
      list += vf
      if (receiver != null) receiver ! vf
    }
    if (receiver != null) receiver ! 'done
    list
  }
}

class SftpConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) {
    sftp.rm(remoteBasePath + what.path)
    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) {
    sftp.put(localBasePath + from.path, remoteBasePath + to.path) // TODO: progressmonitor!
  }
  def getfile(from: VirtualFile, to: VirtualFile) {
    sftp.put(remoteBasePath + from.path, localBasePath + to.path) // TODO: progressmonitor!
  }
  def listrec(subfolder: String, receiver: Actor) = {
    var list = new ListBuffer[VirtualFile]()
    println("searching " + remoteBasePath + subfolder)
    Path.fromString(remoteBasePath + subfolder) ** "*" foreach { ppp =>
      val vf = new VirtualFile {
        path=ppp.path.substring(remoteBasePath.length + 2) // without leading '/'
        modTime = ppp.lastModified
        size = ppp.size.get
        isDir = if (ppp.isDirectory) 1 else 0
      }
      list += vf
      if (receiver != null) receiver ! vf
    }
    if (receiver != null) receiver ! 'done
    list
  }

  import scala.collection.immutable.Map

  class MyUserInfo extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    def getPassword : String = {
      val foo=JOptionPane.showInputDialog(null,"Enter password!")
      foo
    }
    def promptYesNo(str: String) : Boolean = {
//      val options: Array[AnyRef]=Array( "yes", "no" )
//      val foo=JOptionPane.showOptionDialog(null,
//        str,
//        "Warning",
//        JOptionPane.DEFAULT_OPTION,
//        JOptionPane.WARNING_MESSAGE,
//        null, options, options(1))
//      foo==0
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

  override def finalize() {
    println("finalize!!!!")
    super.finalize()
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

