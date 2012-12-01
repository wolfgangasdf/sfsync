package sfsync

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}

import javafx.geometry. {Orientation=>jgo}
import javafx.scene.control. {SelectionMode => jscsm}

import util.Logging
import scalafx.event.ActionEvent
import store._

//import store.MyImplicits._
import synchro._

object Main extends JFXApp with Logging {

  val menu = new Menu("File") {

    items.add(new MenuItem("Open"))
    items.add(new MenuItem("Close"))

  }

  val menuBar = new MenuBar {
    useSystemMenuBar = true
    minWidth = 100
    menus.add(menu)
  }

  var spv : SplitPane = null
  var serverView = new ServerView(Store.config) {
    def onServerChange {
      println("**** onServerchange: " + server.currentProtocol)
      protocolView = new ProtocolView(server)
      subfolderView = new SubFolderView(server)
      spv.items(1) = protocolView
      spv.items(2) = subfolderView
      if (server.currentProtocol > -1) {
        println("****  " + server.currentProtocol)
        protocolView.protocolChanged()
        println("****  " + server.currentProtocol)
      }
      if (server.currentSubFolder > -1) {
        subfolderView.subfolderChanged()
      }
      println("**** /onServerchange: " + server.currentProtocol)
    }
  }
  var protocolView : ProtocolView = null//new ProtocolView(null)
  var subfolderView : SubFolderView = null//= new SubFolderView(null)

  spv = new SplitPane {
    orientation = jgo.VERTICAL
    dividerPositions = (0.3)
    items += (serverView, new BorderPane(), new BorderPane())
  }

  def lookupTextField(id: String) : TextField = {
    val yy = stage.scene.get.lookup(id)
    yy.getClass
    val tf : TextField  = yy.asInstanceOf[javafx.scene.control.TextField]
    tf
  }

  var profile: Profile = null
  val toolBar = new ToolBar {
    content = List(new Button("Compare") {
      onAction = (ae: ActionEvent) => {
        profile = new Profile (
          id = serverView.server.id,
          localFolder = serverView.tfLocalFolder.tf.text.value,
          protocol = new TransferProtocol(
            uri = protocolView.tfURI.tf.text.value,
            basefolder = protocolView.tfBaseFolder.tf.text.value
          ),
          subfolder = subfolderView.tfSubFolder.tf.text.value
        )
        val cl = profile.compare()
        // this gives horrible JRE errors...
//        var st2 = new Stage {
//          scene = new Scene() {
//            content = new BorderPane() {
//              center = new Button("asdf")
//            }
//          }
//        }
//        st2.show
        val cw = new CompareWindow(cl)
        Main.stage.scene().content = cw
        cw.prefWidth <== Main.stage.scene.width

      }
    },
    new Button("Save settings") {
      onAction = (ae: ActionEvent) => {
        Store.save
        println("store saved!")
      }
    },
    new Button("test") {
      onAction = (ae: ActionEvent) => {
        println("spv:" + spv.items )
      }
    }

    )
  }

  val statusBar = new ToolBar {
    content = List(new Label { text = "bla" })

  }


  val maincontent = new VBox() {
    content += menuBar
    content += toolBar
    content += spv
    content += statusBar
  }

  def showContent {
    Main.stage.scene().content = maincontent
  }

  stage = new Stage{
    title = "SFSynchro"
    width = 800
    height = 600
    scene = new Scene {
      //      fill = Color.LIGHTGRAY
      content = maincontent
      onCloseRequest =  {
        // why are these methods called on startup??? disabled for now.
//        Store.save
//        println("close requested" + Store)
      }
    }
  }
  maincontent.prefHeight <== stage.scene.height
  maincontent.prefWidth <== stage.scene.width

  // init
  if (Store.config.currentServer > -1) {
    serverView.serverChanged()
//    if (protocolView.currprotocol > -1) {
//      protocolView.protocolChanged()
//    }
//    if (subfolderView.currsubfolder > -1) {
//      subfolderView.protocolChanged()
//    }
  }



//  mainContent.prefHeight <== stage.scene.height
//  mainContent.prefWidth <== stage.scene.width
//  //  setPrefSize(stage.scene.width.get, stage.scene.height.get)
//
//  //  indicatorPane.prefHeight <== stage.scene.height
//  leftPane.prefWidth <== mainContent.width * 0.2
//  //  controlsPane.prefHeight <== stage.scene.height
//  controlsPane.prefWidth <== mainContent.width * 0.2
//  //  centerPane.prefHeight <== stage.scene.height
//  //  centerPane.prefWidth <== stage.scene.width * 0.6


}

