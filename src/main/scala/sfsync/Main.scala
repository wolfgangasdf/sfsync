package sfsync

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._

import javafx.geometry. {Orientation=>jgo}

import util.Logging
import scalafx.event.ActionEvent
import store._
import javax.swing.JOptionPane
import javafx. {stage => jfxs}
import synchro._
import javafx.application.Platform
import actors.Actor

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
    def onServerChange() {
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
  var cw: CompareWindow = null
  val toolBar = new ToolBar {
    content = List(new Button("Compare") {
      onAction = (ae: ActionEvent) => {
        cw = new CompareWindow()
        Main.stage.scene().content = cw
        cw.prefWidth <== Main.stage.scene.width
        cw.prefHeight <== Main.stage.scene.height
        cw.start()

        profile = new Profile (cw,
          id = serverView.server.id,
          localFolder = serverView.tfLocalFolder.tf.text.value,
          protocol = new TransferProtocol(
            uri = protocolView.tfURI.tf.text.value,
            basefolder = protocolView.tfBaseFolder.tf.text.value
          ),
          subfolder = subfolderView.tfSubFolder.tf.text.value
        )
        cw.setProfile(profile)
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

  def showContent() {
    Main.stage.scene().content = maincontent
  }

  class DialogAlsoNot(msg: String) {
    def showYesNo : Int = {
      JOptionPane.showMessageDialog(null, "alert", "alert", JOptionPane.ERROR_MESSAGE)
      1
    }
  }

  // https://gist.github.com/1887631
  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(Main.stage) // TODO remove
      initModality(jfxs.Modality.APPLICATION_MODAL)
      width = 500
      height = 300
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

  stage = new Stage{
    title = "SFSynchro"
    width = 800
    height = 600
    scene = new Scene {
      //      fill = Color.LIGHTGRAY
      content = maincontent
    }
    onCloseRequest = {
      if (stage != null) { // stupid: this is called at startup???
        println("*************** close requested")
        if (cw != null) cw ! 'done
        if (profile != null) profile.finish()
        Store.save
      }
    }
  }

//  case class showYesNoDialog(msg: String)
//  val dialog = new Dialog
//  class Dialog extends Actor {
//    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
//      initOwner(Main.stage) // TODO remove
//      initModality(jfxs.Modality.APPLICATION_MODAL)
//      width = 500
//      height = 300
//    }
//    def act() {
//      println("actor started")
//      loop {
//        println("before")
//        receive {
//          case showYesNoDialog(msg) => {
//            println("showyn")
//            var res = -1
//            dstage.scene = new Scene {
//              content = new BorderPane {
//                center = new Label { text = msg }
//                bottom = new HBox {
//                  content = List(
//                    new Button("Yes") {
//                      onAction = (ae: ActionEvent) => { res=1; dstage.close }
//                    },
//                    new Button("No") {
//                      onAction = (ae: ActionEvent) => { res=0; dstage.close }
//                    }
//                  )
//                }
//              }
//            }
//            dstage.showAndWait()
//            reply(res==1)
//          }
//        }
//      }
//    }
//  }
//  dialog.start()

  maincontent.prefHeight <== stage.scene.height
  maincontent.prefWidth <== stage.scene.width

  // init

  if (Store.config.currentServer > -1) {
    serverView.serverChanged()
  }
}
