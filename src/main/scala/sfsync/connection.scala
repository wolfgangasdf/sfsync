package sfsync.synchro

import scala.util.matching.Regex
import scala.collection.mutable._
import collection.mutable
import actors.Actor
import actors.Actor._
import scalax.io._
import scalax.file.Path
import scalax.file.PathMatcher.IsFile

class cachedFile(path: String, modTime: Long, size: Long) {

}


case class getfile(from: VirtualFile, to: VirtualFile)
case class putfile(from: VirtualFile, to: VirtualFile)
case class deletefile(what: VirtualFile)
case class listrec(where: String, receiver: Actor)

class LocalConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) = {
    Path.fromString(remoteBasePath + what.path).delete(true)
    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) = {
    Path.fromString(localBasePath + from.path) copyTo Path.fromString(remoteBasePath + to.path)
  }
  def getfile(from: VirtualFile, to: VirtualFile) = {
    Path.fromString(remoteBasePath + from.path) copyTo Path.fromString(localBasePath + to.path)
  }  // the main actor
  def listrec(subfolder: String, receiver: Actor) = {
    var list = new ListBuffer[VirtualFile]()
    println("searching " + remoteBasePath + subfolder)
    // TODO change this to manual search since could be slow smb mount!
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

// TODO: change this to sftp!
class SftpConnection extends GeneralConnection {
  def deletefile(what: VirtualFile) = {
    Path.fromString(remoteBasePath + what.path).delete(true)
    println("deleted " + remoteBasePath + what.path)
  }
  def putfile(from: VirtualFile, to: VirtualFile) = {
    Path.fromString(localBasePath + from.path) copyTo Path.fromString(remoteBasePath + to.path)
  }
  def getfile(from: VirtualFile, to: VirtualFile) = {
    Path.fromString(remoteBasePath + from.path) copyTo Path.fromString(localBasePath + to.path)
  }  // the main actor
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

