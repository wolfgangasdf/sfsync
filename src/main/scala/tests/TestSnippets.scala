package tests

import akka.actor.ActorDSL._
import akka.actor._
import java.security.{Key, KeyStore}
import sfsync.util.Logging

//import akka.actor.{ActorRefFactory, ActorSystem}

//import sfsync.synchro.{ComparedFile, VirtualFile, SftpConnection}

object ActorTest extends App{
  var doit = true
//  val system = ActorSystem("some-api")
  implicit def system = ActorSystem("some-api")
  val act = actor(system)(new Act {
    var doit = true
    become {
      case 'testcase => println("testcase!")
      case 'testexit => { println("testexit!") ; doit = false}
      case 'replyWhenDone => {
        println("replywhendone: doit=" + doit)
        if (!doit) {
          sender ! 'done
          context.stop(self)
        }
      }
    }
  })

  import scala.concurrent.duration
  implicit val recv = inbox()
  act ! 'testcase
  for (iii <- 1 to 10000) act ! 'testcase
  act ! 'testexit
  act ! 'replyWhenDone
  val res1 = recv.receive(duration.FiniteDuration(1,duration.DAYS)); println("res1=" + res1)
//  recv.select()
  //  loopWhile(act.getState!=State.Terminated) {}
  //  println("state " + act.getState.toString)
  //  println("state " + act.getState.toString)
  println("backhere")
  system.shutdown()
  sys.exit()
  //  exit()
}




//import scalax.file._
//import scalax.io._

//object TestTextStore extends App {
//  val pad = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
//  val fff = Path.fromString("/tmp/testtextstore.txt")
//  1 match {
//    case 0 => { //write
//      for (i <- 0 until 100000) {
//        fff.append("line" + i + pad + "\n")
//      }
//    }
//    case 1 => { // read
//    val lll = fff.lines(Line.Terminators.NewLine, includeTerminator = true)
//      val l2 = lll.filter(ff => ff.startsWith("line123"))
//      l2.foreach(ss => println(ss))
//    }
//  }
//
//}

/*
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
*/

//object TestJsch extends App {
//
//  class MyJschLogger extends jsch.Logger {
//    val name = Map(jsch.Logger.DEBUG -> "DEBUG: ",
//      jsch.Logger.INFO -> "INFO: ",
//      jsch.Logger.WARN -> "WARN: ",
//      jsch.Logger.ERROR -> "ERROR: ",
//      jsch.Logger.FATAL -> "FATAL: ")
//
//    def isEnabled(level: Int) = true
//
//    def log(level: Int, message: String) {
//      println(name(level) + message)
//    }
//
//  }
//  jsch.JSch.setLogger(new MyJschLogger)
//
//  var jSch = new jsch.JSch
//
//  val prvkey: Array[Byte] = scalax.file.Path.fromString("/Users/wolle/.ssh/id_dsa").bytes.toArray
//  jSch.addIdentity("wolle",prvkey,null,Array[Byte]())
//  var session = jSch.getSession("wolle", "localhost", 22)
////  jSch.addIdentity("loeffler",prvkey,null,Array[Byte]())
////  var session = jSch.getSession("loeffler", "data01.physics.leidenuniv.nl", 22);
//
//  var ui = new MyUserInfo
//  session.setUserInfo(ui)
//  session.connect()
//  val sftp = session.openChannel("sftp").asInstanceOf[jsch.ChannelSftp]
//
//  sftp.connect(5000)
//  if (sftp.isConnected) {
//    val files = sftp.ls("/tmp/testlocal")
//    println("files=" + files)
//    for (obj <- files) {
//      val lse = obj.asInstanceOf[ChannelSftp#LsEntry]
//      println("file: " + lse.getFilename )
//    }
//    sftp.disconnect()
//  }
//  session.disconnect()
//
//
//  class MyUserInfo extends jsch.UserInfo with jsch.UIKeyboardInteractive {
//    def getPassword() : String = {
//      val foo=JOptionPane.showInputDialog(null,"Enter password!")
//      foo
//    }
//    def promptYesNo(str: String) : Boolean = {
//      val options: Array[AnyRef]=Array( "yes", "no" )
//      val foo=JOptionPane.showOptionDialog(null,
//        str,
//        "Warning",
//        JOptionPane.DEFAULT_OPTION,
//        JOptionPane.WARNING_MESSAGE,
//        null, options, options(1))
//      foo==0
//    }
//
//    def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = null
//
//    def getPassphrase: String = ""
//
//    def promptPassword(message: String): Boolean = { println("prompt pwd") ; true }
//
//    def promptPassphrase(message: String): Boolean = { println("prompt pwd") ; true }
//
//    def showMessage(message: String) { println("message: " + message) }
//  }
//
//}
//
//object TestFileName extends App {
//  Path.fromString("/tmp") ** "*" foreach { ppp => println(ppp.path) }
//}


object TestScalaSubString extends App {
  val s = "asdf"
  println("3=" + s.substring(3))
  println("4=" + s.substring(4))
  println("5=" + s.substring(5))
}


object TestKeystore extends App {
  val ks = KeyStore.getInstance("KeychainStore", "Apple")
  ks.load(null, "x".toCharArray)
//  val sk: javax.crypto.SecretKey = "asdf"
//  val kse = new KeyStore.SecretKeyEntry(sk)
//  val kspp = new KeyStore.PasswordProtection("asdf".toCharArray)
//  ks.setEntry("sfsynctest", kse, kspp)
//  ks.getEntry("sfsynctest", kspp)
//  ks.aliases().foreach(x => println("alias: " + x))
//  ks.setKeyEntry("asdf","asdf".getBytes,null)
  val key = new Key {
  def getEncoded: Array[Byte] = "asdf".getBytes

  def getFormat: String = "asdf"

  def getAlgorithm: String = "SSLv3HMac-SHA"
}
//  ks.setKeyEntry("testsfsync","key".getBytes(),null)
  ks.setKeyEntry("testsfsync",key,"aaa".toCharArray,null)

  println("asdf = " + ks.getKey("newton.pks.mpg.de", "asdf".toCharArray))

//  ks.aliases().foreach(x => {
//    if ( ks.isKeyEntry(x)) {
//      println("alias: " + x + " iskey=" + ks.isKeyEntry(x))
//      println("-> " + ks.getKey(x, "asdf".toCharArray))
//    }
//  })

//  ks.getEntry("newton.pks.mpg.de",null)
//  println("asdf=" + ks.getKey("sfsynctest","".toCharArray))
//  ks.setKeyEntry("sfsynctest","asdf".getBytes(),null)

}

object EncryptionTest extends App {

  class JavaCryptoEncryption(algorithmName: String) {

    import javax.crypto.spec.SecretKeySpec
    import javax.crypto.Cipher
    val b64enc = new sun.misc.BASE64Encoder()
    val b64dec = new sun.misc.BASE64Decoder()
    def encrypt(bytes: String, secret: String): String = {
      val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
      val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
      encipher.init(Cipher.ENCRYPT_MODE, secretKey)
      val res = encipher.doFinal(bytes.getBytes("UTF-8"))
      b64enc.encode(res)
    }

    def decrypt(bytes: String, secret: String): String = {
      val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
      val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
      encipher.init(Cipher.DECRYPT_MODE, secretKey)
      val res = encipher.doFinal(b64dec.decodeBuffer(bytes))
      new String(res, "UTF-8")
    }

  }

  object DES extends JavaCryptoEncryption("DES")

  val e1 = DES.encrypt("secret", "huhupass")
  println(s"e1=$e1")
  val d1 = DES.decrypt("0vmYC8J01+Q=", "huhupass")
  println(s"d1=$d1")
}

class aaa extends Logging {
  info("new class info")
}

object TestLogging extends App with Logging {
  import scala.concurrent.future
  import scala.concurrent.ExecutionContext.Implicits.global
  future {
    info("f1info")
    warn("f1warn")
    error("f1error")
    debug("f1debug")
  }
  future {
    info("f2info")
    warn("f2warn")
    error("f2error")
    debug("f2debug")
  }
  info("info")
  warn("warn")
  error("error")
  debug("debug")
  error("asdf", new Exception())
  println("test other class")
  val a = new aaa
}
