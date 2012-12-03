package sfsync.testsnippets


import scala.actors._
import Actor._
import javax.swing.JOptionPane

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
//  jSch.addIdentity("wolle",prvkey,null,Array[Byte]())
//  var session = jSch.getSession("wolle", "localhost", 22);
  jSch.addIdentity("loeffler",prvkey,null,Array[Byte]())
  var session = jSch.getSession("loeffler", "data01.physics.leidenuniv.nl", 22);

  var ui = new MyUserInfo
  session.setUserInfo(ui);
  session.connect()
  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]

  import scala.collection.JavaConversions._
  sftp.connect(5000)
  if (sftp.isConnected) {
    val files = sftp.ls(".")
    println("files=" + files)
    for (obj <- files) {
      println("file: " + obj)
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
