package util

class StopWatch {
  private val startNanos = System.nanoTime
  var deltaSecs: Double = 0
  def stop { // fast stopping
    deltaSecs = (System.nanoTime - startNanos)/1e9
  }
  def getTime = (System.nanoTime - startNanos)/1e9
  def timeIt = { // a little overhead... 0.13s
    if (deltaSecs == 0) stop
    "%g s" format deltaSecs
  }
  def printTime(msg: String) = {
    println(msg + timeIt)
  }
}

object StopWatch extends StopWatch {
  def timed[T](msg: String)(body: =>T) = { // for use via timed("time=") { body }
  val startNanos = System.nanoTime
    val r = body
    val stopNanos = System.nanoTime
    println(msg + (stopNanos - startNanos)/1e9)
    r
  }
}