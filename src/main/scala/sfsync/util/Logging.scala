package sfsync.util

import java.io.{PrintStream, File}

/*
A truly simple logger trait, just extend "Logger" and use info(),debug(),warn(),error().
Configure using LoggerBase.configure (indicate if a possible configfile will override this).
Specify logger config file location below.
example config file:
  levels=debug,info,warn,error # levels:debug,info,warn,error
  outputs=console # output:console,/tmp/log.txt

 */

object LoggerBase {
  var logInfo = false
  var logDebug = false
  var logWarning = false
  var logError = false
  var logConsole = false
  var logFile = ""
  var outStreams = new scala.collection.mutable.ArrayBuffer[PrintStream]()
  var haveConfigFile = false
  println("Resources root directory: " + getClass.getResource("/"))
  val res = getClass.getResourceAsStream("/sfsync/logconfig.txt")
  if (res != null) {
    haveConfigFile = true
    val sl = scala.io.Source.fromInputStream(res).getLines()
    sl.foreach(s => {
      // DO WITH PATTERN MATCHING!
      val s1 = s.replaceFirst("#.*","")
      if (s1.matches(".*=.*")) {
        val rKeyval =  "\\s*(\\S*)\\s*=\\s*(\\S*)\\s*".r
        val rKeyval(k,v) = s1
        k match {
          case "levels" =>
            if (v.contains("debug")) logDebug = true
            if (v.contains("info")) logInfo = true
            if (v.contains("warn")) logWarning = true
            if (v.contains("error")) logError = true
          case "outputs" =>
            v.split(",").foreach(outp => {
              println("log outp=" + outp)
              if (outp == "console")
                logConsole = true
              else logFile = outp
            })
        }
      }
    })
  }
  if (!haveConfigFile) {
    println("log config file not found, using defaults")
    logWarning = true
    logError = true
    logConsole = true
  }
  def configure(overrideConfigFile: Boolean, logdebug: Boolean, loginfo: Boolean, logwarning: Boolean, logerror: Boolean, logconsole: Boolean, logfile: String = "") {
    var setparms = overrideConfigFile
    if (!overrideConfigFile) {
      if (!haveConfigFile) setparms = true
    }
    if (setparms) {
      logDebug = logdebug
      logInfo = loginfo
      logWarning = logwarning
      logError = logerror
      logConsole = logconsole
      logFile = logfile
      init()
    }
  }
  private def init() {
    outStreams.clear()
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

  protected def threadinfo(msg: String) {
    println("-------------------------------------------- " + msg + " in thread " + Thread.currentThread().getId +
      " (isFX:" + scalafx.application.Platform.isFxApplicationThread + ")")
  }
  def dolog(prefix: String, msg: Any, exc: Throwable) {
    synchronized {
      var logs = prefix + self.getClass.getName + "[" + Thread.currentThread().getId + "]: " + msg
      if (exc != null) logs += "\nException: " + exc.getMessage
      LoggerBase.outStreams.map(ps => ps.println(logs))
      if (exc != null) LoggerBase.outStreams.map(ps => exc.printStackTrace(ps))
    }
  }

}


