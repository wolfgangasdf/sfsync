package sfsync

import java.net.URI

import buildinfo.BuildInfo
import sfsync.util.Logging
import sfsync.store._
import sfsync.synchro._
import sfsync.util.Helpers._

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.concurrent.WorkerStateEvent
import scalafx.scene._
import scalafx.scene.image.Image
import scalafx.scene.control.Alert.AlertType
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.beans.property.StringProperty
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.control.MenuItem._
import javafx.geometry.{Orientation => jgo}
import javafx.{stage => jfxs}


class MainView(filesView: FilesView) extends Tab with Logging {
  this.text = "Settings"
  closable = false
  private val mainView = this

  val serverView: ServerView = new ServerView(Store.config) {
    def onServerChange() {
      val tmpdp =  ArrayBuffer(sp.dividerPositions: _*)
      protocolView = new ProtocolView(server)
      protocolView.prefWidth <== this.width - 10
      subfolderView = new SubFolderView(mainView, server)
      subfolderView.prefWidth <== this.width - 10
      sp.items.set(1, protocolView)
      sp.items.set(2, subfolderView)
      if (server.currentProtocol.value > -1) {
        protocolView.protocolChanged()
      }
      if (server.currentSubFolder.value > -1) {
        subfolderView.subfolderChanged()
      }
      sp.dividerPositions = tmpdp: _*

    }
  }
  var protocolView : ProtocolView = _
  var subfolderView : SubFolderView = _
  val sp = new SplitPane
  sp.orientation = jgo.VERTICAL
  sp.items += (serverView, new BorderPane(), new BorderPane())
  serverView.prefWidth <== sp.width - 10
  content =sp
}

object Main extends JFXApp with Logging {
  import java.io

  // redirect console output, must happen on top of this object!
  private val oldOut = System.out
  private val oldErr = System.err
  var logps: io.FileOutputStream = _
  System.setOut(new io.PrintStream(new MyConsole(false), true))
  System.setErr(new io.PrintStream(new MyConsole(true), true))

  private val logfile = createTempFile("sfsynclog",".txt")
  logps = new io.FileOutputStream(logfile)

  class MyConsole(errchan: Boolean) extends io.OutputStream {
    override def write(b: Int): Unit = {
      runUI { if (logView != null) if (logView.taLog != null) logView.taLog.appendText(b.toChar.toString) }
      if (logps != null) logps.write(b)
      (if (errchan) oldErr else oldOut).print(b.toChar.toString)
    }
  }

  private val VERSION = BuildInfo.version
  private val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  private val version = VERSION + (if (resv != null) " (" + scala.io.Source.fromURL(resv).mkString.trim + ")" else "")

  var settingsView: MainView = _
  var filesView: FilesView = _
  var logView: LogView = _
  var profile: Profile = _

  var cw: FilesView = _
  var tmpse: SyncEntry = _
  var lbInfo: Label = _
  var btCompare: Button = _ // to prevent suspicious forward reference in btSync
  var statusBar: ToolBar = _
  var statusLabel: Label = _
  var btSync: Button = _
  var tabpane: TabPane = _

  stage = new JFXApp.PrimaryStage {
    title = "xxx"
    width = 800
    height = 600
    icons += new Image(getClass.getResource("/icons/icon_16x16.png").toExternalForm)
    icons += new Image(getClass.getResource("/icons/icon_32x32.png").toExternalForm)
    icons += new Image(getClass.getResource("/icons/icon_256x256.png").toExternalForm)
  }

  stage.show()

  if (!DBSettings.getLock) {
    runUI { dialogMessage(AlertType.Error, "SFSync Error", "Lock file exists", "Is another Sfsync instance running?<br>If not, remove " + DBSettings.lockFile.getAbsolutePath) }
    System.exit(1)
  }

  val splash = new Splash

  // I need to return from this so that splash can be updated. initialize in other thread, use runUI{} if needed!
  Future {
    initit(stage)
    Thread.sleep(1500)
    splash.close()
  }

  def initit(myStage: Stage) {
    Checks.CheckComparedFile()

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

    val menuBar = new MenuBar {
      useSystemMenuBar = true
      menus = List(
        new Menu("SFSync") {
          items += new MenuItem("About") {
            onAction = (_: ActionEvent) => {
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
      onAction = (_: ActionEvent) => {
        btCompare.setDisable(true)
        btSync.setDisable(true)
        runSynchronize()
      }
    }
    btCompare = new Button("Compare") {
      onAction = (_: ActionEvent) => {
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
          onAction = (_: ActionEvent) => {
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

    // showing GUI...
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

    info("log file: " + logfile.getAbsolutePath)
    debug("path sep=" + System.getProperty("file.separator"))
  }

  object Status {
    var status: StringProperty = StringProperty("")
    statusLabel.text <== status

    def clear() {
      status.value = ""
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


  private def handleFailed(task: myTask) = {
    runUIwait {
      Main.Status.status.value = "Failed!"
      dialogMessage(AlertType.Error, "Error", task.getTitle, task.getException.toString)
      task.getException.printStackTrace()
      Cache.updateObservableBuffer()
      doCleanup()
    }
  }
  private def handleCancelled() = {
    runUIwait {
      Main.Status.status.value = "Cancelled!"
      Cache.updateObservableBuffer()
      tabpane.selectionModel().select(settingsView)
      doCleanup()
    }
  }

  def runSynchronize() {
    runUIwait {
      Main.Status.status.value = "Synchronize..."
    }
    profile.taskSynchronize.onSucceeded = (_: WorkerStateEvent) => {
      Main.Status.status.value = "Synchronization finished!"
      MyWorker.runTask(profile.taskCleanup)
      runUI {
        tabpane.getSelectionModel.select(0)
        doCleanup()
      }
    }
    profile.taskSynchronize.onFailed = () => handleFailed(profile.taskSynchronize)
    profile.taskSynchronize.onCancelled = () => {
      profile.local.interrupted.set(true)
      profile.remote.interrupted.set(true)
      handleCancelled()
    }
    MyWorker.runTask(profile.taskSynchronize)
  }

  private def runCompare() = {
    Main.Status.status.value = "Compare..."
    doCleanup()
    val sane = settingsView.serverView.server != null && settingsView.serverView.server.localFolder.value != "" &&
      settingsView.protocolView.protocol.protocoluri.value != "" && settingsView.subfolderView.subfolder.subfolders.nonEmpty
    if (sane) {
      profile = new Profile(settingsView.serverView.server, settingsView.protocolView.protocol, settingsView.subfolderView.subfolder)
      lbInfo.text.set("  Current profile:  " + settingsView.serverView.server.toString + " | " + settingsView.subfolderView.subfolder.toString)
      filesView.profile = profile
      filesView.updateSyncButton(allow = false)
      val ctask = new myTask {
        override def call(): Unit = {
          updateTitle("A Compare files")
          updateProgr(0, 100, "Initialize local and remote...")
          profile.taskIni.onSucceeded = () => {
            updateProgr(50, 100, "Run comparison...")

            profile.taskCompFiles.onSucceeded = () => {
              val haveChanges = profile.taskCompFiles.get.asInstanceOf[Boolean]
              Main.btCompare.setDisable(false)
              Cache.updateObservableBuffer()
              debug("havechanges=" + haveChanges)
              val canSync = filesView.updateSyncButton(allow = true)
              if (!haveChanges && canSync) {
                Main.Status.status.value = "Finished compare, no changes found. Synchronizing..."
                runSynchronize()
              } else {
                Main.Status.status.value = "Finished compare"
                tabpane.selectionModel().select(filesView)
                filesView.updateSyncButton(allow = true)
              }
            }
            profile.taskCompFiles.onFailed = () => handleFailed(profile.taskCompFiles)
            profile.taskCompFiles.onCancelled = () => handleCancelled()
            MyWorker.runTask(profile.taskCompFiles)
          }
          profile.taskIni.onFailed = () => handleFailed(profile.taskIni)
          profile.taskIni.onCancelled = () => handleCancelled()
          MyWorker.runTask(profile.taskIni)

          updateProgr(100, 100, "ended!")
        }
      }
      MyWorker.runTask(ctask)

    } else {
      dialogMessage(AlertType.Error, "Error", "Error initializing compare", "Correct sync settings and try again!")
    }
    sane
  }

}
// a scalafx splashscreen: implement from main SFX routine, then call showProgress() or close() from any thread
class Splash extends Logging {
  val maxProgress = 6
  //noinspection VarCouldBeVal
  var progress = 0

  private val sstage = new Stage(jfxs.StageStyle.UNDECORATED) {
    initOwner(Main.stage)
    initModality(jfxs.Modality.APPLICATION_MODAL)
    width = 500
    height = 300
  }

  private val logo = new TextField {
    text = "SFSync"
    prefHeight = 250
    style = "-fx-background-color: lightblue;-fx-font: 100px Tahoma;"
    alignment = Pos.Center
    editable = false
  }
  private val cont = logo

  sstage.scene = new Scene {
    content = cont
  }
  cont.prefWidth <== sstage.getScene.width
  cont.prefHeight <== sstage.getScene.height
  cont.autosize()
  sstage.show()
  sstage.toFront()

  def close() {
    if (progress != maxProgress) {
      info("splashscreen: set maxProgress to " + progress)
    }
    runUI { sstage.close() }
  }
}
