package sfsync

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent

import javafx.geometry. {Orientation=>jgo}

import util.Logging
import scala._
import store._
import javafx.{stage => jfxs}
import synchro._
import scala.concurrent.ops.spawn
import javafx.event.EventHandler
import Helpers._
import akka.actor._

object Helpers {
  def runUI( f: => Unit ) {
    javafx.application.Platform.runLater( new Runnable() {
      def run() {
        f
      }
    })
  }

  def getUnit = {}

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
  def deepCopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
    scala.util.Marshal.load[A](scala.util.Marshal.dump(a))
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
  dividerPositions = (0.3)
  items += (serverView, new BorderPane(), new BorderPane())


}

object Main extends JFXApp with Logging {

  val version = io.Source.fromURL(getClass.getResource("VERSION.txt")).mkString.trim

  val menu = new Menu("File") {

    items.add(new MenuItem("Open"))
    items.add(new MenuItem("Close"))

  }

  def system = ActorSystem("some-api") // TODO: needed?

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

  val toolBar = new ToolBar {
    content = List(new Button("Compare") {
      onAction = (ae: ActionEvent) => {
        doCleanup()

        cw = new CompareWindow()
        Main.stage.scene().content = cw
        cw.prefWidth <== Main.stage.scene.width
        cw.prefHeight <== Main.stage.scene.height
//        cw.start()

        profile = new Profile (cw,mainView.serverView.server, mainView.protocolView.protocol, mainView.subfolderView.subfolder)
        cw.setProfile(profile)
        spawn { // this is key, do in new thread!
          profile.init()
          profile.compare()
        }
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

  def refreshContent = {
    mainView = new MainView
    maincontent.content = List(menuBar,toolBar,mainView,statusBar)
    Main.stage.scene().content = maincontent
    maincontent.prefHeight <== stage.scene.height
    maincontent.prefWidth <== stage.scene.width
    if (Store.config.currentServer.value > -1) {
      mainView.serverView.serverChanged
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "SFSynchro"
    width = 800
    height = 600
    scene = new Scene
    delegate.setOnCloseRequest(new EventHandler[jfxs.WindowEvent] {
      def handle(p1: jfxs.WindowEvent) {
        println("*************** close requested " + stage)
        doClose()
      }
    })
    // this does not work, it is called at startup if contains calls !!!???
//    onCloseRequest = {
//      println("*************** close requested " + stage)
//    }
  }

  import akka.actor.ActorDSL._
  import akka.actor._
  def doCleanup() {
    if (cw != null) cw.act ! 'done
    if (profile != null) profile.finish()
  }
  def doClose() {
    println("*************** close requested")
    doCleanup()
    Store.save()
    sys.exit(0)
  }

  // startup
  println("sfsync version " + version)
  println("java version " + System.getProperty("java.version"))
  println("scala version " + util.Properties.versionString)
  println("javafx version " + System.getProperty("javafx.runtime.version"))

  refreshContent

  // https://gist.github.com/1887631
  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage) // TODO remove
      initModality(jfxs.Modality.APPLICATION_MODAL)
      width = 500
      height = 300
    }
    def showMessage(msg: String) : Boolean = {
      var res = -1
      dstage.scene = new Scene {
        content = new BorderPane {
          center = new Label { text = msg }
          bottom = new HBox {
            content = List(
              new Button("Ok") {
                onAction = (ae: ActionEvent) => { res=1; dstage.close }
              }
            )
          }
        }
      }
      dstage.showAndWait()
      res==1
    }
    def showYesNo(msg: String) : Boolean = {
      var res = -1
      dstage.scene = new Scene {
        content = new BorderPane {
          center = new Label { text = msg }
          bottom = new HBox {
            content = List(
              new Button("Yes") {
                onAction = (ae: ActionEvent) => { res=1; dstage.close }
              },
              new Button("No") {
                onAction = (ae: ActionEvent) => { res=0; dstage.close }
              }
            )
          }
        }
      }
      dstage.showAndWait()
      res==1
    }
    def showInputString(msg: String) : String = {
      var res = ""
      dstage.scene = new Scene {
        content = new BorderPane {
          top = new Label { text = msg }
          center = new TextField {
            text = ""
            onAction = (ae: ActionEvent) => { res = text.value; dstage.close }
          }
        }
      }
      dstage.showAndWait()
      res
    }
  }

}
