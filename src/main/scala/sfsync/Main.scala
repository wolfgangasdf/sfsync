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

import util.Logging
import scala._
import collection.mutable.ArrayBuffer
import store._
import javafx.{stage => jfxs}
import synchro._
import Helpers._
import akka.actor.ActorDSL._
import akka.actor._
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions
import scalafx.geometry.Pos
import java.nio.charset.Charset
import scalafx.beans.property.StringProperty

object Helpers {

  val filecharset = Charset.forName("UTF-8")

  val insetsstd = scalafx.geometry.Insets(5)

  def toJavaPathSeparator(in: String) = {
    if (isWin) in.replaceAll("""\\""", "/")
    else in
  }

  def runUI( f: => Unit ) {
    javafx.application.Platform.runLater( new Runnable() {
      def run() {
        f
      }
    })
  }

  def isMac = System.getProperty("os.name").toLowerCase.contains("mac")
  def isLinux = System.getProperty("os.name").toLowerCase.contains("nix")
  def isWin = System.getProperty("os.name").toLowerCase.contains("win")

  def toHexString(s: String, encoding: String) = {
    s.getBytes(encoding).map("%02x " format _).mkString
  }

  def unit() {}

  def runUIwait( f: => Any ) : Any = {
    var stat: Any = null
    //    synchronized(stat) // not needed??
    val runable = new Runnable() {
      def run() {
        stat = f
      }
    }
    javafx.application.Platform.runLater(runable)
    while(stat == null) { // ugly
      Thread.sleep(0,10000)
    }
    stat
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

class MainView(filesView: FilesView) extends Tab {
  this.text = "Settings"
  closable = false

  var firstStart = true
  var serverView = new ServerView(Store.config) {
    def onServerChange() {
      println("onServerChange!")
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

  val VERSION = "0.1" // TODO: read from build.sbt but how?
  val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  val version = VERSION + (if (resv != null) " (" + io.Source.fromURL(resv).mkString.trim + ")" else "")
  def system = ActorSystem("sfsyncactors")

  var settingsView: MainView = null
  var filesView: FilesView = null
  var profile: Profile = null

  // run checks
  Checks.CheckComparedFile()

  // startup
  println("sfsync version = " + version)
  println("java.version = " + System.getProperty("java.version"))
  println("scala version = " + util.Properties.versionString)
  println("javafx.runtime.version = " + System.getProperty("javafx.runtime.version"))
  println("LC_CTYPE = " + System.getenv("LC_CTYPE"))
  println("(isMac,isLinux,isWin) = " + List(isMac,isLinux,isWin).mkString(","))
  println("settings path = " + DBSettings.settpath)
  if (isMac) {
    if (System.getenv("LC_CTYPE") == null) {
      println("!!!!!!!!!!! set LC_CTYPE variable for correct foreign character handling!")
    }
  }

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

  import org.squeryl.PrimitiveTypeMode.transaction
  import org.squeryl.PrimitiveTypeMode._
  val toolBar = new ToolBar {
    content = List(
      new Button("Compare") {
        onAction = (ae: ActionEvent) => {
          Main.runCompare()
        }
      },
      new Button("Synchronize") {
        onAction = (ae: ActionEvent) => {
          throw new Exception("not impl")
        }
      },
      new Button("Save settings") {
        onAction = (ae: ActionEvent) => {
          Store.save()
          println("store saved!")
        }
      },
      new Button("test: check asdf1") {
        onAction = (ae: ActionEvent) => {

          val res = transaction {
            MySchema.files.where(se => se.path === "asdf1").single
          }
          println("res=" + res)
          unit()
        }
      },
      new Button("test: check local asdf1") {
        onAction = (ae: ActionEvent) => {
          if (tmpse == null) {
            tmpse = transaction {
              MySchema.files.where(se => se.path === "asdf1").single
            }
            println("retrieved se=" + tmpse)
          } else
            println("tmpse=" + tmpse)
          println("persisted: " + tmpse.isPersisted)
          unit()
        }
      },
      new Button("test: update asdf1") {
        onAction = (ae: ActionEvent) => {

          transaction {
            val res = MySchema.files.where(se => se.path === "asdf1").single
            res.lSize += 1
            MySchema.files.update(res)
            println("res after update=" + res)
          }
          unit()
        }
      },
      new Button("test2: updatelistview") {
        onAction = (ae: ActionEvent) => {
//          CacheDB.invalidateCache()
//          CacheDB.updateSyncEntries()
          filesView.updateSyncEntries()
        }
      }
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
  tabpane.prefHeight <== stage.height - menuBar.prefHeight - toolBar.prefHeight - statusBar.prefHeight
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
    println("*************** stop app")
    Store.config.width.value = stage.width.toInt
    Store.config.height.value = stage.height.toInt
    Store.config.dividerPositions = ArrayBuffer(settingsView.sp.dividerPositions: _*)
    Store.save()
    doCleanup()
    sys.exit(0)
  }

  def doCleanup() {
    // TODO
//    if (filesView != null) {
//      filesView.act ! 'done
//      filesView = null
//    }
    if (profile != null) profile.finish()
  }

  def runCompare() {
    doCleanup()
    profile = new Profile (filesView, settingsView.serverView.server, settingsView.protocolView.protocol, settingsView.subfolderView.subfolder)
    filesView.setProfile(profile)
    tabpane.selectionModel().select(filesView)
    future { // this is key, do in new thread!
      try {
        profile.init()
        profile.compare()
      } catch {
        case e: Exception => {
          runUIwait(Main.Dialog.showMessage("Exception: " + e + "\n" + e.getMessage))
          e.printStackTrace()
          runUI {
            doCleanup()
          }
        }
      }
    }
  }

  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage)
      initModality(jfxs.Modality.APPLICATION_MODAL)
      width = 500
      height = 300
    }
    private def showIt(mtype: Int, msg: String) : String  = {
      var res = "-1"
      val cont = new BorderPane {
        style = "-fx-background-color: lightblue;"
        var tf = new TextField {
          text = ""
          onAction = (ae: ActionEvent) => { res = text.value; dstage.close }
        }
        var lab = new Label {
          text = msg
          textAlignment = scalafx.scene.text.TextAlignment.CENTER
        }
        mtype match {
          case 1 | 2 => { center = lab }
          case 3 => { top = lab ; center = tf }
        }
        bottom = new HBox {
          margin = insetsstd
          spacing = 5
          alignment = Pos.CENTER
          content = mtype match {
            case 1 => List(
              new Button("Ok") {
                onAction = (ae: ActionEvent) => { res="1"; dstage.close }
              })
            case 2 => List(
              new Button("Yes") {
                onAction = (ae: ActionEvent) => { res="1"; dstage.close }
              },
              new Button("No") {
                onAction = (ae: ActionEvent) => { res="0"; dstage.close }
              }
            )
            case 3 => List(
              new Button("Ok") {
                onAction = (ae: ActionEvent) => { res=tf.text.value; dstage.close }
              },
              new Button("Cancel") {
                onAction = (ae: ActionEvent) => { res=""; dstage.close }
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

      dstage.showAndWait
      res
    }

    def showMessage(msg: String) : Boolean = {
      val res = showIt(1, msg)
      res == "1"
    }

    def showYesNo(msg: String) : Boolean = {
      val res = showIt(2, msg)
      res == "1"
    }
    def showInputString(msg: String) : String = {
      val res = showIt(3, msg)
      res
    }
  }
}

