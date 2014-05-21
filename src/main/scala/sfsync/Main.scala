package sfsync

import sfsync.util.{LoggerBase, Logging}
import sfsync.store._
import sfsync.synchro._
import sfsync.Helpers._

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx.geometry.Pos
import scalafx.scene.web.WebView
import scalafx.beans.property.StringProperty
import scala._
import scala.language.reflectiveCalls
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import akka.actor._
import scala.collection.mutable.ArrayBuffer

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

  import scalafx.beans.property._
  implicit def StringPropertyToString(sp: StringProperty) = sp.value
  implicit def IntegerPropertyToInt(sp: IntegerProperty) = sp.value
//  implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
//  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)

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
      debug("onServerChange!")
      // connect to database
      CacheDB.connectDB(server.id)
      // update filesview
      filesView.setListItems(CacheDB.syncEntries)
      val tmpdp =  ArrayBuffer(sp.dividerPositions: _*)
      protocolView = new ProtocolView(server)
      subfolderView = new SubFolderView(server)
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
  content =sp
}

object Main extends JFXApp with Logging {
  val VERSION = BuildInfo.version
  val APPNAME = BuildInfo.name
  val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  val version = VERSION + (if (resv != null) " (" + io.Source.fromURL(resv).mkString.trim + ")" else "")
  def system = ActorSystem("sfsyncactors")

  var settingsView: MainView = null
  var filesView: FilesView = null
  var profile: Profile = null

  debug("path sep=" + System.getProperty("file.separator"))

  // logging but allow override by file
  LoggerBase.configure(overrideConfigFile = false, logdebug = false, loginfo = false, logwarning = true, logerror = true, logconsole = true, "")

  var cw: FilesView = null

  var tmpse: SyncEntry = null

  var lbInfo: Label = null

  var btCompare: Button = null // to prevent suspicious forward reference in btSync

  var statusBar: ToolBar = null

  var statusLabel: Label = null

  var btSync: Button = null

  var tabpane: TabPane  = null

  stage = new JFXApp.PrimaryStage {
    title = "xxx"
    width = 800
    height = 600
  }

  stage.show()
  val splash = new Splash

//  threadinfo("Main")

  // I need to return from this so that splash can be updated. initialize in other thread, use runUI{} if needed!
  future {
//    threadinfo("future")
    debug("huhu1")
    splash.showProgress("startup...", 1)
    debug("huhu2")
    initit(stage)
    Thread.sleep(1500)
    splash.close()
  }

  def initit(myStage: Stage) {
//    threadinfo("initit")
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

      import scala.collection.JavaConversions._
      System.getProperties.foreach( p => println("prop " + p.getKey + " : " + p.getValue) )

    splash.showProgress("initializing GUI...", 1)

//    val menu = new Menu("SFSync") {
//      items.add(new MenuItem("About")) // TODO
//      items.add(new MenuItem("Quit")) // TODO
//    }
//    val menuBar = new MenuBar {
//      useSystemMenuBar = true
//      minWidth = 100
//      menus.add(menu)
//    }

    btSync = new Button("Synchronize") {
        onAction = (ae: ActionEvent) => {
          runUI {
            btCompare.setDisable(true)
            btSync.setDisable(true)
          }
          future {
            profile.synchronize()
          }
          unit()
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
            val progress = new Progress("test") {
              onAbortClicked = () => {
                close()
              }
            }
            progress.update(0.3, "aaaadfjklsdfjsldjgldfjgldkfjgldsjfg sdlfkgj sdlfgj sdlfgj lsdfj glskdfj glkdsjf glsdj fglksd")
          }
        },
        lbInfo
      )
    }
    statusBar = new ToolBar {
      statusLabel = new Label() { text = "Sfsync Version " + Main.version }
      content = List(statusLabel)
    }

    tabpane = new TabPane {
    }

    filesView = new FilesView
    settingsView = new MainView(filesView)

    val maincontent = new VBox {
//      if (isMac) content += menuBar // TODO
      content ++= List(toolBar,tabpane,statusBar)
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
      tabpane.tabs = List(settingsView, filesView)
      statusBar.prefWidth <== myStage.width

      if (Store.config.currentServer.value > -1) {
        settingsView.serverView.serverChanged()
      }
    }


    // ini after UI shown
    runUI({
      debug("dp=" + Store.config.dividerPositions.toString)
      if (Store.config.dividerPositions.length > 0)
        settingsView.sp.setDividerPositions(Store.config.dividerPositions: _*)
      else
        settingsView.sp.setDividerPositions(0.3,0.6)
    })
    splash.showProgress("ready.", 1)

  }
  object Status {
    implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
    var status: StringProperty = StringProperty("?")
    var local = StringProperty("?")
    var remote = StringProperty("?")
    List(status,local,remote).map(x => x.onChange(
      statusLabel.text = "Local:" + local.value + "  Remote: " + remote.value + "  | " + status.value
    ))
    def clear() {
      status = ""
      local = ""
      remote = ""
    }
  }



  override def stopApp() {
    info("*************** stop app")
    Store.config.width.value = stage.width.toInt
    Store.config.height.value = stage.height.toInt
    Store.config.dividerPositions = ArrayBuffer(settingsView.sp.dividerPositions: _*)
    Store.save()
    doCleanup()
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

  def runCompare() = {
    doCleanup()
    val sane = settingsView.serverView.server != null && settingsView.serverView.server.localFolder.value != "" &&
      settingsView.protocolView.protocol.protocoluri.value != "" && !settingsView.subfolderView.subfolder.subfolders.isEmpty
    if (sane) {
      profile = new Profile (filesView, settingsView.serverView.server, settingsView.protocolView.protocol, settingsView.subfolderView.subfolder)
      lbInfo.text.set("  Current profile:  " + settingsView.serverView.server.toString + " | " + settingsView.subfolderView.subfolder.toString)
      filesView.profile = profile
      tabpane.selectionModel().select(filesView)
      future { // this is key, do in new thread!
        try {
          profile.init()
          profile.compare()
        } catch {
          case e: Exception =>
            runUIwait(Main.Dialog.showMessage("Exception: " + e + "\n" + e.getMessage))
            e.printStackTrace()
            runUI {
              doCleanup()
            }
        }
      }
    } else {
      Dialog.showMessage("Correct sync settings and try again!")
    }
    sane
  }

  class Progress(title: String) {
    var onAbortClicked = () => {}
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage)
      initModality(jfxs.Modality.APPLICATION_MODAL)
      onCloseRequest = (ae: WindowEvent) => { ae.consume() } // workaround to disable close
      width = 500
      height = 300
    }
    val ti = new TextArea {
      text = title
      editable = false
      prefHeight = 30
      style = "-fx-font-size: 20;"
    }
    val ta = new TextArea {
      text = ""
      editable = false
      wrapText = true
    }
    val pb1 = new ProgressBar { prefHeight = 20 ; tooltip = new Tooltip { text = "Overall" } }
    val pb2 = new ProgressBar { prefHeight = 20 ; tooltip = new Tooltip { text = "File" } }
    val bAbort = new Button("Abort") { prefHeight = 30 ; onAction = (ae: ActionEvent) => { onAbortClicked() } }
    val cont = new VBox {
      style = "-fx-background-color: lightblue;"
      var sp = new ScrollPane {
        content = ta
        fitToWidth = true
        fitToHeight = true
        hbarPolicy = ScrollPane.ScrollBarPolicy.NEVER
        vgrow = Priority.ALWAYS
      }
      content ++= List(ti, sp, bAbort, pb1, pb2)
    }
    dstage.scene = new Scene {
      content = cont
    }
    cont.prefWidth <== dstage.scene.width
    cont.prefHeight <== dstage.scene.height
    pb1.prefWidth <== dstage.scene.width
    pb2.prefWidth <== dstage.scene.width
    bAbort.prefWidth <== dstage.scene.width
    cont.autosize()
    dstage.show()

    def update(progressValue: Double, s: String) {
      ta.text = s
      pb1.progress = progressValue
    }
    def updateProgressBar2(progressValue: Double) {
      pb2.progress = progressValue
    }

    def close() { dstage.close() }
  }


  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage)
      initModality(jfxs.Modality.APPLICATION_MODAL)
      onCloseRequest = (ae: WindowEvent) => { ae.consume() } // workaround to disable close
      width = 500
      height = 300
    }
    private def showIt(mtype: Int, msg: String, htmlmsg: String = "") : String  = {
      var res = "-1"
      val cont = new VBox {
        style = "-fx-background-color: lightblue;"
        var tf = new TextField {
          text = ""
          onAction = (ae: ActionEvent) => { res = text.value; dstage.close() }
        }
        var sp = new ScrollPane { // text message
          content = new TextArea {
            text = msg
            editable = false
          }
          fitToWidth = true
          fitToHeight = true
          if (htmlmsg != "") prefHeight = 15
        }

        var sp2 = new ScrollPane { // optional html message
          content = new WebView {
            engine.loadContent(htmlmsg)
          }
          fitToWidth = true
          fitToHeight = true
        }

        content.add(sp)
        mtype match {
          case 1 | 2 => if (htmlmsg != "") content.add(sp2)
          case 3 => content.add(tf)
        }

        import scalafx.scene.layout.HBox._ // implicit conversion must be imported??!
        content += new HBox {
          prefHeight = 25
          margin = insetsstd
          spacing = 5
          alignment = Pos.CENTER
          content = mtype match {
            case 1 => List(
              new Button("Ok") {
                onAction = (ae: ActionEvent) => { res="1"; dstage.close() }
              })
            case 2 => List(
              new Button("Yes") {
                onAction = (ae: ActionEvent) => { res="1"; dstage.close() }
              },
              new Button("No") {
                onAction = (ae: ActionEvent) => { res="0"; dstage.close() }
              }
            )
            case 3 => List(
              new Button("Ok") {
                onAction = (ae: ActionEvent) => { res=tf.text.value; dstage.close() }
              },
              new Button("Cancel") {
                onAction = (ae: ActionEvent) => { res=""; dstage.close() }
              }
            )
          }
        }
      }
      dstage.scene = new Scene {
        content = cont
      }
      cont.prefWidth <== dstage.scene.width
      cont.prefHeight <== dstage.scene.height
      cont.autosize()
      dstage.showAndWait()
      res
    }

    def showMessage(msg: String, htmlmsg: String = "") : Boolean = {
      val res = showIt(1, msg, htmlmsg)
      res == "1"
    }

    def showYesNo(msg: String, htmlmsg: String = "") : Boolean = {
      val res = showIt(2, msg, htmlmsg)
      res == "1"
    }
    def showInputString(msg: String) : String = {
      val res = showIt(3, msg)
      res
    }
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
    alignment = Pos.CENTER
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
    content ++= List(logo,ta,pb)
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
    ta.text = text
    progress += increment
    runUIwait { pb.progress = progress.toDouble/maxProgress }
  }

  def close() {
    if (progress != maxProgress) {
      info("splashscreen: set maxProgress to " + progress)
    }
    runUI { sstage.close() }
  }
}
