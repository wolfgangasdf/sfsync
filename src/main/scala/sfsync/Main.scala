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

object Helpers {

  val insetsstd = scalafx.geometry.Insets(5)

  def runUI( f: => Unit ) {
    javafx.application.Platform.runLater( new Runnable() {
      def run() {
        f
      }
    })
  }

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

class MainView extends SplitPane {
  var serverView = new ServerView(Store.config) {
    def onServerChange() {
      protocolView = new ProtocolView(server)
      subfolderView = new SubFolderView(server)
      items(1) = protocolView
      items(2) = subfolderView
      if (server.currentProtocol.value > -1) {
        protocolView.protocolChanged()
      }
      if (server.currentSubFolder.value > -1) {
        subfolderView.subfolderChanged()
      }
    }
  }
  var protocolView : ProtocolView = null
  var subfolderView : SubFolderView = null

  orientation = jgo.VERTICAL
  items += (serverView, new BorderPane(), new BorderPane())
}

object Main extends JFXApp with Logging {

  val VERSION = "0.1" // TODO: read from build.sbt but how?
  val resv = getClass.getResource("/sfsync/HGVERSION.txt")
  val version = VERSION + (if (resv != null) " (" + io.Source.fromURL(resv).mkString.trim + ")" else "")

  // run checks
  Checks.CheckComparedFile()

  val menu = new Menu("File") {
    items.add(new MenuItem("Open"))
    items.add(new MenuItem("Close"))
  }

  def system = ActorSystem("sfsyncactors")

  val menuBar = new MenuBar {
    useSystemMenuBar = true
    minWidth = 100
    menus.add(menu)
  }

//  var spv : SplitPane = null

  def lookupTextField(id: String) : TextField = {
    val yy = stage.scene.get.lookup(id)
    yy.getClass
    val tf : TextField  = yy.asInstanceOf[javafx.scene.control.TextField]
    tf
  }

  var profile: Profile = null
  var cw: CompareWindow = null
  var mainView: MainView = null
  var maincontent = new VBox

  def runCompare() {
    doCleanup()
    cw = new CompareWindow()
    Main.stage.scene().content = cw
    cw.prefWidth <== Main.stage.scene.width
    cw.prefHeight <== Main.stage.scene.height
    profile = new Profile (cw,mainView.serverView.server, mainView.protocolView.protocol, mainView.subfolderView.subfolder)
    cw.setProfile(profile)
    future { // this is key, do in new thread!
      try {
        profile.init()
        profile.compare()
      } catch {
        case e: Exception => {
          runUIwait(Dialog.showMessage(e.getMessage))
          doCleanup()
          refreshContent()
        }
      }
    }
  }

  val toolBar = new ToolBar {
    content = List(new Button("Compare") {
      onAction = (ae: ActionEvent) => {
        runCompare()
      }
    },
    new Button("Save settings") {
      onAction = (ae: ActionEvent) => {
        Store.save()
        println("store saved!")
      }
    },
      new Button("test") {
        onAction = (ae: ActionEvent) => {
        }
      },
      new Button("test2") {
        onAction = (ae: ActionEvent) => {
        }
      }
    )
  }

  val statusBar = new ToolBar {
    content = List(new Label { text = "Sfsync Version " + version })
  }

  def refreshContent() {
    mainView = new MainView
    maincontent.content = List(menuBar,toolBar,mainView,statusBar)
    Main.stage.scene().content = maincontent
    maincontent.prefHeight <== stage.scene.height
    maincontent.prefWidth <== stage.scene.width
    mainView.prefHeight <== stage.scene.height - menuBar.prefHeight - toolBar.prefHeight - statusBar.prefHeight
    if (Store.config.currentServer.value > -1) {
      mainView.serverView.serverChanged()
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "SFSynchro"
    width = Store.config.width.toDouble
    height = Store.config.height.toDouble
    scene = new Scene
  }

  override def stopApp() {
    println("*************** stop app")
    doCleanup()
    Store.config.width.value = stage.width.toInt
    Store.config.height.value = stage.height.toInt
    Store.config.dividerPositions = ArrayBuffer(mainView.dividerPositions: _*)
    Store.save()
    sys.exit(0)
  }

  def doCleanup() {
    if (cw != null) cw.act ! 'done
    if (profile != null) profile.finish()
  }

  // startup
  println("sfsync version " + version)
  println("java version " + System.getProperty("java.version"))
  println("scala version " + util.Properties.versionString)
  println("javafx version " + System.getProperty("javafx.runtime.version"))
  println("LC_CTYPE=" + System.getenv("LC_CTYPE"))
  System.getProperty("os.name") match {
    case "Mac OS X" => {
      if (System.getenv("LC_CTYPE") == null) {
        println("!!!!!!!!!!! set LC_CTYPE variable for correct foreign character handling!")
      }
    }
  }
  refreshContent()

  // UI initialization: is executed after UI shown
  runUI({
    mainView.dividerPositions = Store.config.dividerPositions: _*
  })


  // https://gist.github.com/1887631
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

