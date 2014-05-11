package sfsynctests

import akka.actor.Actor
import sfsync.util.StopWatch

object TestListRecSpeed1java extends App {
  var numfiles = 0
  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
    //    println("searching " + remoteBasePath + "/" + subfolder)
    def parseContent(folder: String) : Unit = {
      val files = new java.io.File(folder).listFiles()
      for (ff <- files) {
//        val vf = new VirtualFile(ff.getPath, ff.lastModified(), ff.length, if (ff.isDirectory) 1 else 0)
//        if (!vf.fileName.matches(filterregexp)) {
//          println("file: " + vf.path)
//          list += vf
//          numfiles += 1
//          //          if (receiver != null) receiver ! vf
//          if (ff.isDirectory) {
//            parseContent(ff.getPath)
//          }
//        }
      }
    }
    parseContent(subfolder)
    //    if (receiver != null) receiver ! 'done
    //    list
    numfiles
  }
  val sw1 = new StopWatch
  val nf = listrec("/Unencrypted_Data/tempnospotlight/teststorelargelocal","",null)
  println("loaded local list1 (" + nf + ") in " + sw1.stopGetTimeString )
}

object TestListRecSpeed2 extends App {
  def listrec() = {
    for (cc <- 1 to 20000) {
//      val vf = new VirtualFile("",cc,0,0) // 0.19s
      //      val vf = "asdf" // 0.134s
    }
  }
  val sw1 = new StopWatch
  listrec()
  sw1.stop()
  println("loaded local list1 in " + sw1.stopGetTimeString )
}

object TestFindSpeed extends App {
//  val list = new ListBuffer[VirtualFile]()
//  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
//  for (ii <- 1 to 20000) {
////    val vf = new VirtualFile("asdf"+ii, 0,ii,0)
//  }
//  StopWatch.timed("find in ") {
//    for (ii <- 1 to 20000) {
//      val asdf = list.find(x => x.path == "asdf"+ii).getOrElse(null)
////      val cf = new ComparedFile(asdf, new VirtualFile("asdf"+ii, 0,ii,0), new VirtualFile("asgdf"+ii, 0,ii,0))
////      comparedfiles += cf
//    }
//  }

}
