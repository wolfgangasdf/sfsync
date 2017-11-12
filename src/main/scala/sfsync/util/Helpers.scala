package sfsync.util

import java.io.File
import java.nio.charset.Charset
import java.nio.{charset => jnc}
import java.util.{concurrent => juc}

import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{ButtonType, DialogEvent, _}
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.Includes._
import scalafx.geometry.Rectangle2D
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.web.WebView
import scalafx.stage.Screen

object Helpers {

  // for debugging, this throws exceptions at a place depending on number
  // mind that certain settings have to be chosen (e.g., sftp/local file) to see it fail.
  // after MyWorker etc changes, test all if exceptions propagate as intended!
  val failat = 0 // 0..5 currently

  val filecharset: Charset = jnc.Charset.forName("UTF-8")

  val insetsstd = scalafx.geometry.Insets(5)

  val directoryFilter = "([a-zA-Z]:)?/.*" // not for sftp... if (isWin) ".:/.*" else "/.*"

  def toJavaPathSeparator(in: String): String = {
    if (isWin) in.replaceAll("""\\""", "/")
    else in
  }

  def isMac: Boolean = System.getProperty("os.name").toLowerCase.contains("mac")
  def isLinux: Boolean = System.getProperty("os.name").toLowerCase.matches("(.*nix)|(.*nux)")
  def isWin: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

  def createTempFile(prefix: String, suffix: String): File = { // standard io.File.createTempFile points often to strange location
  val tag = System.currentTimeMillis().toString
    var dir = System.getProperty("java.io.tmpdir")
    if (Helpers.isLinux || Helpers.isMac) if (new java.io.File("/tmp").isDirectory)
      dir = "/tmp"
    new java.io.File(dir + "/" + prefix + "-" + tag + suffix)
  }

  def toHexString(s: String, encoding: String): String = {
    s.getBytes(encoding).map("%02x " format _).mkString
  }

  def tokMGTPE(d: Double): String = {
    var num = d
    var ext = ""
    val expo = math.min((Math.log(d) / Math.log(1000)).floor.toInt, 6)
    if (expo > 0) {
      ext = "kMGTPE" (expo - 1).toString
      num = d / math.pow(1000, expo)
    }
    val res = "%.1f%s".format(num, ext)
    res
  }

  def unit() {}

  def runUI( f: => Unit ) {
    if (!scalafx.application.Platform.isFxApplicationThread) {
      scalafx.application.Platform.runLater(() => {
        f
      })
    } else {
      f
    }
  }

  def runUIwait( f: => Any) : Any = {
    if (!scalafx.application.Platform.isFxApplicationThread) {
      @volatile var stat: Any = null
      val runnable = new Runnable() {
        def run() {
          stat = f
        }
      }
      val future = new juc.FutureTask[Any](runnable, null)
      scalafx.application.Platform.runLater( future )
      future.get()
      stat
    } else {
      f
    }
  }

  def dialogOkCancel(titletext: String, header: String, content: String): Boolean = {
    new Alert(AlertType.Confirmation) {
      //initOwner(stage)
      title = titletext
      headerText = header
      contentText = content
    }.showAndWait() match {
      case Some(ButtonType.OK) => true
      case _ => false
    }
  }

  def dialogInputString(titletext: String, header: String, content: String): String = {
    new TextInputDialog() {
      //initOwner(stage)
      title = titletext
      headerText = header
      contentText = content
    }.showAndWait() match {
      case Some(s) => s
      case _ => ""
    }
  }

  def dialogMessage(alertType: AlertType, titletext: String, header: String, htmlmsg: String) {
    new Dialog[Boolean] {
      //if (stage.owner.nonEmpty) initOwner(stage)
      title = titletext
      headerText = header
      private val sp2 = new ScrollPane { // optional html message
        content = new WebView {
          engine.loadContent(htmlmsg)
        }
        fitToWidth = true
        fitToHeight = true
      }
      dialogPane().content = sp2
      dialogPane().buttonTypes = Seq(ButtonType.OK)
    }.showAndWait()
  }


  object MyWorker {
    val taskList = new ObservableBuffer[myTask]()
    private val taskListView = new ListView[myTask] {
      items = taskList
      cellFactory = {lv =>
        new ListCell[myTask] {
          item.onChange({
            if (item.value != null) {
              val title = new Label {
                wrapText = true
                text <== item.value.titleProperty
                prefWidth <== lv.width - 180
                hgrow = Priority.Always
              }
              val message = new Label {
                wrapText = true
                prefWidth <== lv.width - 30
                text <== item.value.messageProperty
                style = "-fx-font-size: 10"
              }
              val progress = new ProgressBar {
                prefWidth = 150
                progress <== item.value.progressProperty
              }
              val hb = new HBox {
                children ++= Seq(title, progress)
              }
              val vb = new VBox {
                children ++= Seq(hb, message)
                fillWidth = true
              }
              graphic = vb
            } else {
              graphic = null
            }
          })
        }
      }
    }
    private val al = new Dialog[javafx.scene.control.ButtonType] {
      // not needed? initOwner(Main.stage)
      title = "Progress"
      resizable = true
      dialogPane.value.content = new VBox { children ++= Seq(new Label("Tasks:"), taskListView) }
      dialogPane.value.getButtonTypes += ButtonType.Cancel
      val sb: Rectangle2D = Screen.primary.visualBounds
      dialogPane.value.setPrefSize(sb.width/2.5, sb.height/3)
    }

    al.onCloseRequest = (_: DialogEvent) => {
      if (taskList.nonEmpty) {
        taskList.foreach(t => if (t.isRunning) t.cancel())
        println("cancelled all tasks!")
      }
    }

    var backgroundTimer: java.util.Timer = _ // just to clean up finished tasks
    al.showing.onChange{ (_, _, newv) =>
      if (newv) {
        val ttask = new java.util.TimerTask {
          override def run(): Unit = {
            if (taskList.nonEmpty) scalafx.application.Platform.runLater(() => {
              var iii = 0
              while (iii < taskList.length) {
                if (taskList.get(iii).isDone || taskList.get(iii).isCancelled)
                  taskList.remove(iii)
                else
                  iii += 1
              }
              if (taskList.isEmpty) {
                al.close()
              }
            })
          }
        }
        backgroundTimer = new java.util.Timer()
        backgroundTimer.schedule(ttask, 0, 500)
      } else {
        backgroundTimer.cancel()
      }
    }

    def runTask(atask: myTask): Unit = {
      scalafx.application.Platform.runLater(() => {
        if (!al.showing.value) al.show()
        taskList.add(atask)
        println("added task " + atask)
        val th = new Thread(atask)
        th.setDaemon(true)
        th.start()
      })
    }
  }

  abstract class myTask extends javafx.concurrent.Task[Any] {
    def updateProgr(workDone: Double, max: Double, msg: String): Unit = {
      updateMessage(msg)
      updateProgress(workDone, max)
    }
  }

}
