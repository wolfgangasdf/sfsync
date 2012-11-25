package util

trait Logging {
  protected def debug(msg: => Any): Unit = dolog(msg)
  protected def debug(msg: => Any, t: => Throwable): Unit = dolog(msg, t)
  protected def error(msg: => Any): Unit = dolog(msg)
  protected def error(msg: => Any, t: => Throwable): Unit = dolog(msg, t)
  protected def info(msg: => Any): Unit = dolog(msg)
  protected def info(msg: => Any, t: => Throwable): Unit = dolog(msg, t)
  protected def warn(msg: => Any): Unit = dolog(msg)
  protected def warn(msg: => Any, t: => Throwable): Unit = dolog(msg, t)

  def dolog(msg: Any): Unit = {
    println(msg)
  }

  def dolog(msg: Any, exc: Throwable): Unit = {
    println(msg)
    exc.getMessage
    exc.printStackTrace
  }

}