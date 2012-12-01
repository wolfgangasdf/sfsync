package sfsync.synchro

import java.io.File
import scala.util.matching.Regex
import scala.collection.mutable._
import collection.mutable


class cachedFile(path: String, modTime: Long, size: Long) {

}

class LocalConnection extends GeneralConnection {
  def copyFile(from: File, to: String) {
    val out = new java.io.BufferedWriter( new java.io.FileWriter(to) )
    io.Source.fromFile(from).getLines.foreach(s => out.write(s,0,s.length))
    out.close()
  }
  def getFile(fromPath: String, toPath: String) = {
    copyFile(new java.io.File(basePath+"/"+fromPath), toPath)
  }

  import scala.util.matching.Regex
  def recursiveListLocalFiles(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    if (these != null) {
      val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
      good ++ these.filter(_.isDirectory).flatMap(recursiveListLocalFiles(_,r))
    } else {
      new ArrayBuffer[File]().toArray
    }
  }
  def listRecursively(path: String): List[VirtualFile] = {
    var fa = recursiveListLocalFiles(new File(basePath+"/"+path), filterregex)
    var vfl = new ListBuffer[VirtualFile]()
//    vfl += new VirtualFile("adf",1,2)
    fa.foreach(fff => {vfl += new VirtualFile {
        path=fff.getAbsolutePath.substring((basePath+"/").length+2)
        modTime = fff.lastModified()
        size = fff.length()
        isDir = if (fff.isDirectory) 1 else 0
      }
    })
    vfl.sorted.toList
  }

  def putFile(fromPath: String, toPath: String) {
    copyFile(new java.io.File(fromPath), basePath+"/"+toPath)
  }
}

trait GeneralConnection {
//  var conntype: ConnType.Value
  var cachedList: Array[File] = null
  var basePath: String = ""
  var filterregex: Regex = new Regex(""".*""")
  def getFile(fromPath: String, toPath: String)
  def putFile(fromPath: String, toPath: String)
  def listRecursively(path: String): List[VirtualFile]
  // TODO: time diff thing

}

class VirtualFile extends Ordered[VirtualFile] {
  var path: String = ""
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

