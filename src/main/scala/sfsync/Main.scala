package sfsync

import java.net.URI

import sfsync.util.Logging
import sfsync.store._
import sfsync.synchro._
import sfsync.Helpers._

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.concurrent.WorkerStateEvent
import scalafx.scene._
import scalafx.scene.control.Alert.AlertType
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.web.WebView
import scalafx.beans.property.StringProperty
import scala.language.reflectiveCalls
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.control.MenuItem._

import javafx.geometry. {Orientation=>jgo}
import javafx.{stage => jfxs}
import java.nio.charset.Charset
import java.util.concurrent.FutureTask


object Helpers {

  val filecharset = Charset.forName("UTF-8")

  val insetsstd = scalafx.geometry.Insets(5)

  val directoryFilter = if (isWin) ".:/.*" else "/.*"

  def toJavaPathSeparator(in: String) = {
    if (isWin) in.replaceAll("""\\""", "/")
    else in
  }

  def isMac = System.getProperty("os.name").toLowerCase.contains("mac")
  def isLinux = System.getProperty("os.name").toLowerCase.contains("nix")
  def isWin = System.getProperty("os.name").toLowerCase.contains("win")

  def toHexString(s: String, encoding: String) = {
    s.getBytes(encoding).map("%02x " format _).mkString
  }

  def unit() {}

  def runUI( f: => Unit ) {
    if (!scalafx.application.Platform.isFxApplicationThread) {
      scalafx.application.Platform.runLater( new Runnable() {
        def run() {
          f
        }
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
      val future = new FutureTask[Any](runnable, null)
      scalafx.application.Platform.runLater( future )
      future.get()
      stat
    } else {
      f
    }
  }

  // this only works for serializable objects (no javafx properties), would be useful for copy server/proto/set!
//  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
//    scala.util.Marshal.load[A](scala.util.Marshal.dump(a))
}

class MainView(filesView: FilesView) extends Tab with Logging {
  this.text = "Settings"
  closable = false

  var firstStart = true
  var serverView = new ServerView(Store.config) {
    def onServerChange() {
      val tmpdp =  ArrayBuffer(sp.dividerPositions: _*)
      protocolView = new ProtocolView(server)
      protocolView.prefWidth <== this.width - 10
      subfolderView = new SubFolderView(server)
      subfolderView.prefWidth <== this.width - 10
      sp.items(1) = protocolView
      sp.items(2) = subfolderView
      if (server.currentProtocol.value > -1) {
        protocolView.protocolChanged()
      }
      if (server.currentSubFolder.value > -1) {
        subfolderView.subfolderChanged()
      }
      sp.dividerPositions = tmpdp: _*

    }
  }
  var protocolView : ProtocolView = null
  var subfolderView : SubFolderView = null
  val sp = new SplitPane
  sp.orientation = jgo.VERTICAL
  sp.items += (serverView, new BorderPane(), new BorderPane())
  serverView.prefWidth <== sp.width - 10
  content =sp
}

object Main extends JFXApp with Logging {
  import java.io

  // redirect console output, must happen on top of this object!
  val oldOut = System.out
  val oldErr = System.err
  var logps: io.FileOutputStream = null
  System.setOut(new io.PrintStream(new MyConsole(false), true))
  System.setErr(new io.PrintStream(new MyConsole(true), true))

  val logfile = createTempFile("sfsynclog",".txt")
  logps = new io.FileOutputStream(logfile)

  class MyConsole(errchan: Boolean) extends io.OutputStream {
    override def write(b: Int): Unit = {
      runUI { if (logView != null) if (logView.taLog != null) logView.taLog.appendText(b.toChar.toString) }
      if (logps != null) logps.write(b)
      (if (errchan) oldErr else oldOut).print(b.toChar.toString)
    }
  }

  def createTempFile(prefix: String, suffix: String) = { // standard io.File.createTempFile points often to strange location
  val tag = System.currentTimeMillis().toString
    var dir = System.getProperty("java.io.tmpdir")
    if (Helpers.isLinux || Helpers.isMac) if (new io.File("/tmp").isDirectory)
      dir = "/tmp"
    new io.File(dir + "/" + prefix + "-" + tag + suffix)
  }



  val VERSION = BuildInfo.version
  val APPNAME = BuildInfo.name
  val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  val version = VERSION + (if (resv != null) " (" + scala.io.Source.fromURL(resv).mkString.trim + ")" else "")

  var settingsView: MainView = null
  var filesView: FilesView = null
  var logView: LogView = null
  var profile: Profile = null

  var cw: FilesView = null
  var tmpse: SyncEntry = null
  var lbInfo: Label = null
  var btCompare: Button = null // to prevent suspicious forward reference in btSync
  var statusBar: ToolBar = null
  var statusLabel: Label = null
  var btSync: Button = null
  var tabpane: TabPane = null

  stage = new JFXApp.PrimaryStage {
    title = "xxx"
    width = 800
    height = 600
  }

  stage.show()

  if (!DBSettings.getLock) {
    runUI { dialogMessage(AlertType.Error, "SFSync Error", "Lock file exists", "Is another Sfsync instance running?<br>If not, remove " + DBSettings.lockFile.getAbsolutePath) }
    System.exit(1)
  }

  val splash = new Splash

  // I need to return from this so that splash can be updated. initialize in other thread, use runUI{} if needed!
  Future {
    splash.showProgress("startup...", 1)
    initit(stage)
    Thread.sleep(1500)
    splash.close()
  }

  def initit(myStage: Stage) {
    splash.showProgress("running checks...", 1)
    Checks.CheckComparedFile()
    splash.showProgress("startup...", 1)
    info("sfsync version = " + version)
    info("java.version = " + System.getProperty("java.version"))
    info("scala version = " + scala.util.Properties.versionString)
    info("javafx.runtime.version = " + System.getProperty("javafx.runtime.version"))
    info("LC_CTYPE = " + System.getenv("LC_CTYPE"))
    info("(isMac,isLinux,isWin) = " + List(isMac, isLinux, isWin).mkString(","))
    info("settings path = " + DBSettings.settpath)
    if (isMac) {
      if (System.getenv("LC_CTYPE") == null) {
        warn("!!!!!!!!!!! set LC_CTYPE variable for correct foreign character handling!")
      }
    }

    splash.showProgress("initializing GUI...", 1)

    val menuBar = new MenuBar {
      useSystemMenuBar = true
      menus = List(
        new Menu("SFSync") {
          items += new MenuItem("About") {
            onAction = (ae: ActionEvent) => {
              import java.awt.Desktop
              if (Desktop.isDesktopSupported) {
                val desktop = Desktop.getDesktop
                if (desktop.isSupported(Desktop.Action.BROWSE)) {
                  desktop.browse(new URI("https://bitbucket.org/wolfgang/sfsynctest"))
                }
              }
            }
          }
        }
      )
    }

    btSync = new Button("Synchronize") {
      onAction = (ae: ActionEvent) => {
        btCompare.setDisable(true)
        btSync.setDisable(true)
        runSynchronize()
      }
    }
    btCompare = new Button("Compare") {
      onAction = (ae: ActionEvent) => {
        runUI {
          btSync.setDisable(true)
          btCompare.setDisable(true)
        }
        if (!Main.runCompare()) {
          btCompare.setDisable(false)
        }
      }
    }
    btSync.setDisable(true)

    lbInfo = new Label()

    val toolBar = new ToolBar {
      content = List(
        btCompare,
        btSync,
        new Button("test") {
          onAction = (ae: ActionEvent) => {
          }
        },
        lbInfo
      )
    }
    statusBar = new ToolBar {
      statusLabel = new Label() {
        text = "Sfsync Version " + Main.version
      }
      content = List(statusLabel)
    }

    tabpane = new TabPane {
    }

    filesView = new FilesView
    settingsView = new MainView(filesView)
    logView = new LogView

    val maincontent = new VBox {
      //      if (isMac) content += menuBar
      children ++= List(menuBar, toolBar, tabpane, statusBar)
    }

    splash.showProgress("showing GUI...", 1)
    runUI {
      myStage.title = "SFSync"
      myStage.x = Store.config.x.toDouble
      myStage.y = Store.config.y.toDouble
      myStage.width = Store.config.width.toDouble
      myStage.height = Store.config.height.toDouble
      myStage.scene = new Scene {
        content = maincontent
      }

      maincontent.prefHeight <== myStage.height
      maincontent.prefWidth <== myStage.width
      tabpane.prefHeight <== myStage.height - toolBar.height - statusBar.height - 21
      tabpane.tabs = List(settingsView, filesView, logView)
      statusBar.prefWidth <== myStage.width

      if (Store.config.currentServer.value > -1) {
        settingsView.serverView.serverChanged()
      }
    }


    // ini after UI shown
    runUI({
      debug("dp=" + Store.config.dividerPositions.toString)
      if (Store.config.dividerPositions.nonEmpty)
        settingsView.sp.setDividerPositions(Store.config.dividerPositions: _*)
      else
        settingsView.sp.setDividerPositions(0.3, 0.6)
    })
    splash.showProgress("ready.", 1)
    info("log file: " + logfile.getAbsolutePath)
    debug("path sep=" + System.getProperty("file.separator"))
  }

  object Status {
    implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)

    var status: StringProperty = StringProperty("?")
    statusLabel.text <== status

    def clear() {
      status = ""
    }
  }


  override def stopApp() {
    info("*************** stop app")
    Store.config.width.value = stage.width.toInt
    Store.config.height.value = stage.height.toInt
    Store.config.dividerPositions = ArrayBuffer(settingsView.sp.dividerPositions: _*)
    Store.save()
    doCleanup()
    DBSettings.releaseLock()
    sys.exit(0)
  }

  def doCleanup() {
    lbInfo.text.set("")
    btCompare.setDisable(false)
    btSync.setDisable(true)
    if (profile != null) {
      profile = null
    }
  }

  object MyWorker {
    val taskList = new ObservableBuffer[myTask]()
    val taskListView = new ListView[myTask] {
      items = taskList
      cellFactory = {lv =>
        new ListCell[myTask] {
          item.onChange({
            if (item.value != null) {
              val title = new Label {
                text <== item.value.titleProperty
                maxWidth = Double.MaxValue
                hgrow = Priority.Always
              }
              val message = new Label {
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
    val al = new Dialog[javafx.scene.control.ButtonType] {
      initOwner(Main.stage)
      title = "Progress"
      resizable = true
      dialogPane.value.content = new VBox { children ++= Seq(new Label("Tasks:"), taskListView) }
      dialogPane.value.getButtonTypes += ButtonType.Cancel
      dialogPane.value.setPrefSize(480, 320)
    }

    al.onCloseRequest = (de: DialogEvent) => {
      if (taskList.nonEmpty) {
        taskList.foreach(t => if (t.isRunning) t.cancel())
        println("cancelled all tasks!")
      }
    }

    var backgroundTimer: java.util.Timer = null // just to clean up finished tasks
    al.showing.onChange{ (_, oldv, newv) =>
      if (newv) {
        val ttask = new java.util.TimerTask {
          override def run(): Unit = {
            if (taskList.nonEmpty) scalafx.application.Platform.runLater( new Runnable() {
            def run() {
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
      scalafx.application.Platform.runLater( new Runnable() {
      def run() {
        if (!al.showing.value) al.show()
        taskList.add(atask)
        println("added task " + atask)
        val th = new Thread(atask)
        th.setDaemon(true)
        th.start()
      }
      })
    }
  }

  abstract class myTask extends javafx.concurrent.Task[Any] {
    def updateProgr(workDone: Double, max: Double, msg: String): Unit = {
      updateMessage(msg)
      updateProgress(workDone, max)
    }
  }

  def handleFailed(task: myTask) = {
    runUI {
      dialogMessage(AlertType.Error, "Error", task.getTitle, task.getException.toString)
      task.getException.printStackTrace()
      Cache.updateObservableBuffer()
      doCleanup()
    }
  }
  def handleCancelled() = {
    runUI {
      Cache.updateObservableBuffer()
      doCleanup()
    }
  }

  def runSynchronize() {
    runUIwait {
      Main.Status.clear()
    }
    profile.taskSynchronize.onSucceeded = (wse: WorkerStateEvent) => {
      Main.Status.status.value = "Synchronization finished!"
      MyWorker.runTask(profile.taskCleanup)
      runUI {
        tabpane.getSelectionModel.select(0)
        doCleanup()
      }
    }
    profile.taskSynchronize.onFailed = (wse: WorkerStateEvent) => handleFailed(profile.taskSynchronize)
    profile.taskSynchronize.onCancelled = () => handleCancelled()
    MyWorker.runTask(profile.taskSynchronize)
  }

  def runCompare() = {
    doCleanup()
    val sane = settingsView.serverView.server != null && settingsView.serverView.server.localFolder.value != "" &&
      settingsView.protocolView.protocol.protocoluri.value != "" && settingsView.subfolderView.subfolder.subfolders.nonEmpty
    if (sane) {
      profile = new Profile(settingsView.serverView.server, settingsView.protocolView.protocol, settingsView.subfolderView.subfolder)
      lbInfo.text.set("  Current profile:  " + settingsView.serverView.server.toString + " | " + settingsView.subfolderView.subfolder.toString)
      filesView.profile = profile
      tabpane.selectionModel().select(filesView)
      filesView.updateSyncButton(allow = false)
      val ctask = new myTask {
        override def call(): Unit = {
          updateTitle("A Compare files")
          updateProgr(0, 100, "Initialize local and remote...")
          profile.taskIni.onSucceeded = (wse: WorkerStateEvent) => {
            updateProgr(50, 100, "Run comparison...")

            profile.taskCompFiles.onSucceeded = (wse: WorkerStateEvent) => {
              debug("comp: succeeded")
              val haveChanges = profile.taskCompFiles.get.asInstanceOf[Boolean]
              Main.btCompare.setDisable(false)
              debug("comp: succeededB")
              Cache.updateObservableBuffer()
              debug("comp: succeededC")
              debug("havechanges=" + haveChanges)
              val canSync = filesView.updateSyncButton(allow = true)
              if (!haveChanges && canSync) {
                Main.Status.status.value = "Finished compare, no changes found. Synchronizing..."
                runSynchronize()
              } else {
                Main.Status.status.value = "Finished compare"
                filesView.updateSyncButton(allow = true)
              }
            }
            profile.taskCompFiles.onFailed = () => handleFailed(profile.taskCompFiles)
            profile.taskCompFiles.onCancelled = () => handleCancelled()
            MyWorker.runTask(profile.taskCompFiles)
          }
          profile.taskIni.onFailed = () => handleFailed(profile.taskIni)
          MyWorker.runTask(profile.taskIni)

          updateProgr(100, 100, "ended!")
        }
      }
      ctask.onCancelled
      MyWorker.runTask(ctask)

    } else {
      dialogMessage(AlertType.Error, "Error", "Error initializing compare", "Correct sync settings and try again!")
    }
    sane
  }

  def dialogOkCancel(titletext: String, header: String, content: String): Boolean = {
    new Alert(AlertType.Confirmation) {
      initOwner(stage)
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
      initOwner(stage)
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
      if (stage.owner.nonEmpty) initOwner(stage)
      title = titletext
      headerText = header
      var sp2 = new ScrollPane { // optional html message
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

}
// a scalafx splashscreen: implement from main SFX routine, then call showProgress() or close() from any thread
class Splash extends Logging {
  val maxProgress = 6
  var progress = 0

  val sstage = new Stage(jfxs.StageStyle.UNDECORATED) {
    initOwner(Main.stage)
    initModality(jfxs.Modality.APPLICATION_MODAL)
    width = 500
    height = 300
  }

  val logo = new TextField {
    text = "SFSync"
    prefHeight = 250
    style = "-fx-background-color: lightblue;-fx-font: 100px Tahoma;"
    alignment = Pos.Center
    editable = false
  }
  val ta = new TextArea {
    text = ""
    prefHeight = 35
    editable = false
    wrapText = true
  }
  val pb = new ProgressBar {
    prefHeight = 15
    tooltip = new Tooltip { text = "Progress" }
  }
  val cont = new VBox {
    children ++= List(logo,ta,pb)
  }
  sstage.scene = new Scene {
    content = cont
  }
  cont.prefWidth <== sstage.scene.width
  cont.prefHeight <== sstage.scene.height
  pb.prefWidth <== cont.width
  cont.autosize()
  sstage.show()
  sstage.toFront()

  def showProgress(text: String, increment: Int) {
//    threadinfo("showprogr")
    progress += increment
    runUIwait {
      ta.text = text
      pb.progress = progress.toDouble/maxProgress
    }
  }

  def close() {
    if (progress != maxProgress) {
      info("splashscreen: set maxProgress to " + progress)
    }
    runUI { sstage.close() }
  }
}
