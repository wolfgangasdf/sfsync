package sfsync.util

// all in seconds!
class StopWatch {
  var deltaSecs: Double = 0
  private var startNanos = System.nanoTime
  def doit(interval: Double): Boolean = {
    if (getTime > interval) {
      restart()
      true
    } else false
  }
  def getTime: Double = (System.nanoTime - startNanos)/1e9
  def getTimeRestart: Double = {
    val x = getTime
    restart()
    x
  }
  def stopGetTimeString: String = { // a little overhead... 0.13s
    if (deltaSecs == 0) stop()
    "%g s" format deltaSecs
  }
  def stopPrintTime(msg: String): Unit = {
    println(msg + stopGetTimeString)
  }
  def printLapTime(msg: String): Unit = {
    println(msg + getTime)
  }
  def restart() {
    deltaSecs = 0
    startNanos = System.nanoTime
  }
  def stop() { // fast stopping
    deltaSecs = (System.nanoTime - startNanos)/1e9
  }
}

object StopWatch extends StopWatch {
  def timed[T](msg: String)(body: =>T): T = { // for use via timed("time=") { body }
  val startNanos = System.nanoTime
    val r = body
    val stopNanos = System.nanoTime
    println(msg + (stopNanos - startNanos)/1e9)
    r
  }
}