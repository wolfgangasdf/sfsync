package sfsync.util

import java.io.{PrintStream, File}

/*
A truly simple logger trait, just mixin "Logger" and use info(),debug(),warn(),error()
specify config file location below
without config file: just set LoggerBase.logInfo = true ... and call LoggerBase.init()
example config file:

# list attributes that are logged such as
# levels:debug,info,warn,error
levels=debug,info,warn,error
# output:console,/tmp/log.txt
outputs=console

 */

object LoggerBase {
  var logInfo = false
  var logDebug = false
  var logWarning = false
  var logError = false
  var logConsole = false
  var logFile = ""
  var outStreams = new scala.collection.mutable.ArrayBuffer[PrintStream]()
  var haveConfig = false
  val res = getClass.getResourceAsStream("/sfsync/logconfig.txt")
  if (res != null) {
    haveConfig = true
    val sl = scala.io.Source.fromInputStream(res).getLines()
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
              println("log outp=" + outp)
              if (outp == "console")
                logConsole = true
              else logFile = outp
            })
          }
        }
      }
    })
  }
  if (!haveConfig) {
    println("log config file not found, using defaults")
    logWarning = true
    logError = true
    logConsole = true
  }
  def init() {
    if (logConsole) outStreams += Console.out
    if (logFile != "") {
      val f = new File(logFile)
      outStreams += new PrintStream(f)
    }
    println("log config: " + List(logDebug, logInfo, logWarning, logError, logConsole, logFile))
  }
  init()

}

trait Logging {
  self =>

  protected def debug(msg: => Any, t: => Throwable = null) { if (LoggerBase.logDebug) dolog("DEB: ", msg, t) }
  protected def error(msg: => Any, t: => Throwable = null) { if (LoggerBase.logError) dolog("ERR: ", msg, t) }
  protected def info(msg: => Any, t: => Throwable = null) { if (LoggerBase.logInfo) dolog("INF: ", msg, t) }
  protected def warn(msg: => Any, t: => Throwable = null) { if (LoggerBase.logWarning) dolog("WRN: ", msg, t) }

  def dolog(prefix: String, msg: Any, exc: Throwable) {
    synchronized {
      var logs = prefix + self.getClass.getName + "[" + Thread.currentThread().getId + "]: " + msg
      if (exc != null) logs += "\nException: " + exc.getMessage
      LoggerBase.outStreams.map(ps => ps.println(logs))
      if (exc != null) LoggerBase.outStreams.map(ps => exc.printStackTrace(ps))
    }
  }

}


