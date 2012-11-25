package util

/**
 * Created with IntelliJ IDEA.
 * User: wolle
 * Date: 14.10.2012
 * Time: 18:47
 * To change this template use File | Settings | File Templates.
 */
  import java.io._

class FileHelper(file : File) {
  def write(text : String) : Unit = {
    val fw = new FileWriter(file)
    try{ fw.write(text) }
    finally{ fw.close }
  }
  def foreachLine(proc : String=>Unit) : Unit = {
    val br = new BufferedReader(new FileReader(file))
    try{ while(br.ready) proc(br.readLine) }
    finally{ br.close }
  }
  def deleteAll : Unit = {
    def deleteFile(dfile : File) : Unit = {
      if(dfile.isDirectory) {
        println("deleting " + dfile + " recursively")
        dfile.listFiles.foreach{ f => deleteFile(f) }
      }
      dfile.delete
    }
    deleteFile(file)
  }
}

object FileHelper{
  implicit def file2helper(file : File) = new FileHelper(file)
}

