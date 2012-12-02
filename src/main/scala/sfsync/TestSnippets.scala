package sfsync.testsnippets


import scala.actors._
import Actor._
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

