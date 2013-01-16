package sfsync.testsnippets


import scala.actors._
import Actor._
import javax.swing.JOptionPane
import com.jcraft.jsch.ChannelSftp._
import sfsync.synchro.{ComparedFile, VirtualFile, SftpConnection}
import collection.mutable.ListBuffer
import util.StopWatch._
import actors.!

object ActorTest extends App {
  var act = actor {
    var doit = true
    loopWhile(doit) {
      println("before")
      receive {
        case 'testcase => println("testcase!")
        case 'testexit => { println("testexit!") ; doit = false}
        case 'replyWhenDone => {
          reply('done)
          exit // important, program hangs without!
        }
      }
    }
    println("behindloop!")
  }
  act ! 'testcase
  act ! 'testcase
  act !? 'replyWhenDone
  //  loopWhile(act.getState!=State.Terminated) {}
  //  println("state " + act.getState.toString)
  //  println("state " + act.getState.toString)
  println("backhere")
  //  exit()
}




import scalax.file._
import scalax.io._

object TestTextStore extends App {
  val pad = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  val fff = Path.fromString("/tmp/testtextstore.txt")
  1 match {
    case 0 => { //write
      for (i <- 0 until 100000) {
        fff.append("line" + i + pad + "\n")
      }
    }
    case 1 => { // read
    val lll = fff.lines(Line.Terminators.NewLine,true)
      val l2 = lll.filter(ff => ff.startsWith("line123"))
      l2.foreach(ss => println(ss))
    }
  }

}


import fr.janalyse.ssh.SSH
object TestJanalysessh extends App {

  SSH.once(host = "localhost", username = "wolle") {
    ssh =>

//      val uname = ssh executeAndTrim "uname -a"
//      val fsstatus = ssh execute "df -m"
//      val fmax = ssh get "/proc/sys/fs/file-max"

//      ssh.shell {
//        sh => // For higher performances
//          val hostname = sh.executeAndTrim("hostname")
//          val files = sh.execute("find /usr/lib/")
//      }
      ssh.ftp {
        ftp => // For higher performances
          val cpuinfo = ftp.get("/tmp/test")
          println("cpu: " + cpuinfo )


      }
      // output streaming
//      def receiver(data: Option[String]) {
//        data foreach {
//          println(_)
//        }
//      }
//      val executor = ssh.run("vmstat 1 3", receiver)
//      executor.waitForEnd
  }
}


import com.jcraft.jsch
import jsch.ChannelSftp
import scala.collection.JavaConversions._

object TestJsch extends App {

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
  jsch.JSch.setLogger(new MyJschLogger)

  var jSch = new jsch.JSch

  val prvkey: Array[Byte] = scalax.file.Path.fromString("/Users/wolle/.ssh/id_dsa").bytes.toArray
  jSch.addIdentity("wolle",prvkey,null,Array[Byte]())
  var session = jSch.getSession("wolle", "localhost", 22);
//  jSch.addIdentity("loeffler",prvkey,null,Array[Byte]())
//  var session = jSch.getSession("loeffler", "data01.physics.leidenuniv.nl", 22);

  var ui = new MyUserInfo
  session.setUserInfo(ui);
  session.connect()
  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]

  sftp.connect(5000)
  if (sftp.isConnected) {
    val files = sftp.ls("/tmp/testlocal")
    println("files=" + files)
    for (obj <- files) {
      val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
      println("file: " + lse.getFilename )
    }
    sftp.disconnect()
  }
  session.disconnect()


  class MyUserInfo extends jsch.UserInfo with jsch.UIKeyboardInteractive {
    def getPassword() : String = {
      val foo=JOptionPane.showInputDialog(null,"Enter password!")
      foo
    }
    def promptYesNo(str: String) : Boolean = {
      val options: Array[AnyRef]=Array( "yes", "no" )
      val foo=JOptionPane.showOptionDialog(null,
        str,
        "Warning",
        JOptionPane.DEFAULT_OPTION,
        JOptionPane.WARNING_MESSAGE,
        null, options, options(1))
      foo==0
    }

    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null

    def getPassphrase: String = ""

    def promptPassword(message: String): Boolean = { println("prompt pwd") ; true }

    def promptPassphrase(message: String): Boolean = { println("prompt pwd") ; true }

    def showMessage(message: String) { println("message: " + message) }
  }

}

object TestFileName extends App {
  Path.fromString("/tmp") ** "*" foreach { ppp => println(ppp.path) }
}

import util.StopWatch

object TestListRecSpeed extends App {
  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
    //    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: Path) : Unit = {
      //      println("parsing " + folder)
      for (cc <- folder.children().toList.sorted) { // sorted slow but faster for cache find
      val vf = new VirtualFile {
          path=cc.path
          modTime = cc.lastModified
          size = cc.size.get
          isDir = if (cc.isDirectory) 1 else 0
        }
        if ( !vf.fileName.matches(filterregexp) ) {
          list += vf
          if (receiver != null) receiver ! vf
          if (cc.isDirectory) {
            parseContent(cc)
          }
        }
      }
    }
    val sp = Path.fromString(subfolder)
    println("sp=" + sp)
    if (sp.exists) parseContent(sp)
    if (receiver != null) receiver ! 'done
    list
  }
  val sw1 = new StopWatch
  listrec("/Unencrypted_Data/tempnospotlight/teststorelargelocal","",null)
  println("loaded local list in " + sw1.timeIt )
}

object TestListRecSpeed1 extends App {
  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
    //    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    var numfiles = 0
    def parseContent(folder: Path) : Unit = {
      //      println("parsing " + folder)
      //      for (cc <- folder.children().toList.sorted) { // sorted slow but faster for cache find
      for (cc <- folder.children().toList) { // sorted costs 0.7 sec, toList nothing
        val vf = new VirtualFile(cc.path, cc.lastModified, cc.size.get, if (cc.isDirectory) 1 else 0) // 0.3sec!!!
        if (!vf.fileName.matches(filterregexp)) {
          list += vf
//          if (receiver != null) receiver ! vf
          if (cc.isDirectory) {
            parseContent(cc)
          }
          numfiles += 1
        }
      }
    }
    val sp = Path.fromString(subfolder)
    //    println("sp=" + sp)
    if (sp.exists) parseContent(sp)
    //    if (receiver != null) receiver ! 'done
    //    list
    numfiles
  }
  val sw1 = new StopWatch
  val nf = listrec("/Unencrypted_Data/tempnospotlight/teststorelargelocal","",null)
  println("loaded local list1 (" + nf + ") in " + sw1.timeIt )
}

object TestListRecSpeed1java extends App {
  var numfiles = 0
  def listrec(subfolder: String, filterregexp: String, receiver: Actor) = {
    //    println("searching " + remoteBasePath + "/" + subfolder)
    val list = new ListBuffer[VirtualFile]()
    def parseContent(folder: String) : Unit = {
      val files = (new java.io.File(folder)).listFiles()
      for (ff <- files) {
      val vf = new VirtualFile(ff.getPath, ff.lastModified(), ff.length, if (ff.isDirectory) 1 else 0)
        if (!vf.fileName.matches(filterregexp)) {
          println("file: " + vf.path)
          list += vf
          numfiles += 1
          //          if (receiver != null) receiver ! vf
          if (ff.isDirectory) {
            parseContent(ff.getPath)
          }
        }
      }
    }
    parseContent(subfolder)
    //    if (receiver != null) receiver ! 'done
    //    list
    numfiles
  }
  val sw1 = new StopWatch
  val nf = listrec("/Unencrypted_Data/tempnospotlight/teststorelargelocal","",null)
  println("loaded local list1 (" + nf + ") in " + sw1.timeIt )
}

object TestListRecSpeed2 extends App {
  def listrec() = {
    for (cc <- 1 to 20000) {
      val vf = new VirtualFile("",cc,0,0) // 0.19s
//      val vf = "asdf" // 0.134s
    }
  }
  val sw1 = new StopWatch
  listrec()
  sw1.stop
  println("loaded local list1 in " + sw1.timeIt )
}

object TestFindSpeed extends App {
  val list = new ListBuffer[VirtualFile]()
  var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
  for (ii <- 1 to 20000) {
    val vf = new VirtualFile("asdf"+ii, 0,ii,0)
  }
  StopWatch.timed("find in ") {
    for (ii <- 1 to 20000) {
      val asdf = list.find(x => x.path == "asdf"+ii).getOrElse(null)
      val cf = new ComparedFile(asdf, new VirtualFile("asdf"+ii, 0,ii,0), new VirtualFile("asgdf"+ii, 0,ii,0))
      comparedfiles += cf
    }
  }

}

object TestScalaSubString extends App {
  val s = "asdf"
  println("3=" + s.substring(3))
  println("4=" + s.substring(4))
  println("5=" + s.substring(5))
}