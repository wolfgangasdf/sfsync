package sfsync

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scala.language.reflectiveCalls

import javafx.geometry. {Orientation=>jgo}

import util.{LoggerBase, Logging}
import scala._
import collection.mutable.ArrayBuffer
import store._
import javafx.{stage => jfxs}
import synchro._
import Helpers._
import akka.actor._
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scalafx.geometry.Pos
import java.nio.charset.Charset
import scalafx.beans.property.StringProperty
import java.util.concurrent.FutureTask
import scalafx.scene.web.WebView

object Helpers {

  val filecharset = Charset.forName("UTF-8")

  val insetsstd = scalafx.geometry.Insets(5)

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

  // this only works for serializable objects (no javafx properties)
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
  val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  val version = VERSION + (if (resv != null) " (" + io.Source.fromURL(resv).mkString.trim + ")" else "")
  def system = ActorSystem("sfsyncactors")

  var settingsView: MainView = null
  var filesView: FilesView = null
  var profile: Profile = null

  // logging but allow override by file
  LoggerBase.configure(overrideConfigFile = false, logdebug = false, loginfo = false, logwarning = true, logerror = true, logconsole = true, "")

  // run checks
  Checks.CheckComparedFile()
  // startup
  info("sfsync version = " + version)
  info("java.version = " + System.getProperty("java.version"))
  info("scala version = " + scala.util.Properties.versionString)
  info("javafx.runtime.version = " + System.getProperty("javafx.runtime.version"))
  info("LC_CTYPE = " + System.getenv("LC_CTYPE"))
  info("(isMac,isLinux,isWin) = " + List(isMac,isLinux,isWin).mkString(","))
  info("settings path = " + DBSettings.settpath)
  if (isMac) {
    if (System.getenv("LC_CTYPE") == null) {
      warn("!!!!!!!!!!! set LC_CTYPE variable for correct foreign character handling!")
    }
  }

//  import scala.collection.JavaConversions._
//  System.getProperties.foreach( p => println("prop " + p.getKey + " : " + p.getValue) )

  // init

  val menu = new Menu("File") {
    items.add(new MenuItem("Open"))
    items.add(new MenuItem("Close"))
  }

  val menuBar = new MenuBar {
    useSystemMenuBar = true
    minWidth = 100
    menus.add(menu)
  }

  var cw: FilesView = null

  var tmpse: SyncEntry = null

  var btCompare: Button = null // to prevent suspicious forward reference in btSync
  val btSync: Button = new Button("Synchronize") {
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

  val lbInfo = new Label()

  val toolBar = new ToolBar {
    content = List(
      btCompare,
      btSync,
      new Button("Save settings") {
        onAction = (ae: ActionEvent) => {
          Store.save()
        }
      },
      new Button("Stop") {
        onAction = (ae: ActionEvent) => {
          Main.doStop()
        }
      },
    lbInfo
//      new Button("testX") {
//        onAction = (ae: ActionEvent) => {
//          unit()
//        }
//      }
//      new Button("test") {
//        onAction = (ae: ActionEvent) => {
//          unit()
//        }
//      },
//      new Button("test info entry") {
//        onAction = (ae: ActionEvent) => {
//          val se = filesView.tv.selectionModel.get().getSelectedItem
//          println("se = " + se)
//          unit()
//        }
//      },
//      new Button("test: updatelistview") {
//        onAction = (ae: ActionEvent) => {
//          filesView.updateSyncEntries()
//        }
//      }
    )
  }

  object Status {
    var status: StringProperty = StringProperty("?")
    var local = StringProperty("?")
    var remote = StringProperty("?")
    List(status,local,remote).map(x => x.onChange(
      statusBar.lab.text = "Local:" + local.value + "  Remote: " + remote.value + "  | " + status.value
    ))
  }

  val statusBar = new ToolBar {
    var lab = new Label() { text = "Sfsync Version " + Main.version }
    content = List(lab)
  }

  val tabpane = new TabPane {
  }

  var maincontent = new VBox {
    content = List(menuBar,toolBar,tabpane,statusBar)
  }


  filesView = new FilesView
  settingsView = new MainView(filesView)

  stage = new JFXApp.PrimaryStage {
    title = "SFSync"
    width = Store.config.width.toDouble
    height = Store.config.height.toDouble
    scene = new Scene {
      content = maincontent
    }
  }

  maincontent.prefHeight <== stage.height
  maincontent.prefWidth <== stage.width
  tabpane.prefHeight <== stage.height - menuBar.height - toolBar.height - statusBar.height - 21
  tabpane.tabs = List(settingsView, filesView)
  statusBar.prefWidth <== stage.width

  if (Store.config.currentServer.value > -1) {
    settingsView.serverView.serverChanged()
  }

  // ini after UI shown
  runUI({
    settingsView.sp.setDividerPositions(Store.config.dividerPositions: _*)
  })

  override def stopApp() {
    info("*************** stop app")
    Store.config.width.value = stage.width.toInt
    Store.config.height.value = stage.height.toInt
    Store.config.dividerPositions = ArrayBuffer(settingsView.sp.dividerPositions: _*)
    Store.save()
    doCleanup()
    sys.exit(0)
  }

  def doStop() {
    if (profile != null) {
      profile.stop()
    }
    doCleanup()
  }

  def doCleanup() {
    lbInfo.text.set("")
    btCompare.setDisable(false)
    btSync.setDisable(true)
    if (profile != null) {
      profile = null
    }
  }

  var threadCompare: Thread = null

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
        threadCompare = Thread.currentThread()
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

  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage)
      initModality(jfxs.Modality.APPLICATION_MODAL)
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

