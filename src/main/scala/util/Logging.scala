package util

import java.nio.file.{Files, Paths}
import java.nio.charset.Charset
import java.io.{PrintWriter, PrintStream, File}
import scala.collection.JavaConversions._

trait Logging {
  self =>

  var logInfo = false
  var logDebug = false
  var logWarning = false
  var logError = false
  var logConsole = false
  var logFile = ""
  var outStreams = new scala.collection.mutable.ArrayBuffer[PrintStream]()

  protected def debug(msg: => Any, t: => Throwable = null): Unit = if (logDebug) dolog("DEB: ", msg, t)
  protected def error(msg: => Any, t: => Throwable = null): Unit = if (logError) dolog("ERR: ", msg, t)
  protected def info(msg: => Any, t: => Throwable = null): Unit = if (logInfo) dolog("INF: ", msg, t)
  protected def warn(msg: => Any, t: => Throwable = null): Unit = if (logWarning) dolog("WRN: ", msg, t)

  def dolog(prefix: String, msg: Any, exc: Throwable) {
    synchronized {
      var logs = prefix + self.getClass + "[" + Thread.currentThread().getId + "]: " + msg
      if (exc != null) logs += "\nException: " + exc.getMessage
      outStreams.map(ps => ps.println(logs))
      if (exc != null) outStreams.map(ps => exc.printStackTrace(ps))
    }
  }

  val res = getClass.getResource("/logconfig.txt")
  if (res != null) {
    val fff = Paths.get(res.getFile)
    if (Files.exists(fff)) {
      val sl = Files.readAllLines(fff, Charset.forName("UTF-8"))
      sl.foreach(s => {
        // DO WITH PATTERN MATCHING!
        s.replaceFirst("#.*","")
        if (s.matches(".*=.*")) {
          val rKeyval =  "\\s*(\\S*)\\s*=\\s*(\\S*)\\s*".r
          val rKeyval(k,v) = s
          k match {
            case "levels" => {
              if (v.contains("debug")) logDebug = true
              if (v.contains("info")) logInfo = true
              if (v.contains("warning")) logWarning = true
              if (v.contains("error")) logError = true
            }
            case "outputs" => {
              v.split(",").foreach(outp => {
                println("outp=" + outp)
                if (outp == "console")
                  logConsole = true
                else logFile = outp
              })
            }
          }
        }
      })
    }
  }
  if (logConsole) outStreams += Console.out
  if (logFile != "") {
    val f = new File(logFile)
    outStreams += new PrintStream(f)
  }
  println("log config: " + List(logDebug, logInfo, logWarning, logError, logConsole, logFile))

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