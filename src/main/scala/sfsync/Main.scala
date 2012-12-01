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

object TestSynchro extends App  with Logging {
  debug("asdf")


  var ppp = new Profile (
    name = "testprofile",
    localFolder = "/tmp/testlocal",
    protocol = new TransferProtocol (
      name = "protlocalfolder",
      conntype = ConnType.Local,
      basefolder = "/tmp/testremote"
    ),
    subfolder = "."

  )

  ppp.synchronize()


}


class MyListView[T](val factory: () => T = null, newitems: sfxc.ObservableBuffer[T], currIdx: Int, val onChange: () => Unit ) extends VBox {
  minHeight=120
  println("lv(" + factory.getClass + "): " + currIdx)
  var lvs = new control.ListView[T]() {
    items = newitems
    minHeight = 100
    selectionModel.get().clearSelection()
    selectionModel.get().select(currIdx)
    selectionModel.get().getSelectedItems.onChange()
  }
//  println("sel: " + lvs.selectionModel.get().getSelectedItems.head)
  content = List(
    lvs,
    new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            lvs.items.get.add(factory())
            println("asdf")
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            lvs.items.get().remove(lvs.selectionModel.get().getSelectedItem)
            println("asdf")
          }
        }
      )
    })
}

class MyTextField(labelText: String) extends HBox {
  var tf = new TextField() {
    prefWidth = 500
    text = "..."
  }
  content = List(
    new Label() { text = labelText },
    tf
  )
}

abstract class ServerView(val config: Config) extends BorderPane {
  def onServerChange()
  var server: Server = null
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer)
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { server.name = tf.text.value} }
  var tfLocalFolder = new MyTextField("Local folder: ") { tf.onAction = (ae: ActionEvent) => { server.localFolder = tf.text.value} }
//  lvs.lvs.getSelectionModel.getSelectedItems.onChange( (aaa,bbb) => {
  lvs.lvs.getSelectionModel.getSelectedIndices.onChange( (aaa,bbb) => {
    // show server details
    server=lvs.lvs.items.get.get(aaa.head)
    config.currentServer = lvs.lvs.items.get().indexOf(server)
    tfName.tf.text = server.name
    tfLocalFolder.tf.text = server.localFolder
    onServerChange()
    println("lvs changed")
  })

  top = new Label() { text = "Servers:" }
  left = lvs
  right = new VBox() { content = List(tfName, tfLocalFolder) }
}

class ProtocolView(val server: Server) extends BorderPane {
  var protocol: Protocol = null
  val protocols = if (server!=null) server.protocols else new sfxc.ObservableBuffer[Protocol]
  val currprotocol = if (server!=null) server.currentProtocol else -1
  var lvp = new MyListView[Protocol](() => new Protocol,protocols, currprotocol)
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { protocol.name = tf.text.value} }
  var tfBaseFolder = new MyTextField("Base folder: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocolbasefolder = tf.text.value} }
  var tfURI = new MyTextField("Protocol URI: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocoluri = tf.text.value} }
  lvp.lvs.getSelectionModel.getSelectedItems.onChange( (aaa,bbb) => {
    // show protocol details
    protocol=aaa.head
    if (protocol!=null) {
      println("protocol=" + protocol)
      server.currentProtocol = lvp.lvs.items.get().indexOf(protocol)
      tfName.tf.text = protocol.name
      tfBaseFolder.tf.text = protocol.protocolbasefolder
      tfURI.tf.text = protocol.protocoluri
      println("lvp changed")
    } else {
      server.currentProtocol = -1
      tfName.tf.text = "..."
      tfBaseFolder.tf.text = "..."
      tfURI.tf.text = "..."
    }
  })
  top = new Label() { text = "Protocols:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfURI, tfBaseFolder) }
}

class SubFolderView(val server: Server) extends BorderPane {
  var subfolder: SubFolder = null
  val subfolders = if (server!=null) server.subfolders else new sfxc.ObservableBuffer[SubFolder]
  val currsubfolder = if (server != null) server.currentSubFolder else -1
  var lvp = new MyListView[SubFolder](() => new SubFolder,subfolders, currsubfolder)
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { subfolder.name = tf.text.value} }
  var tfSubFolder = new MyTextField("Subfolder: ") { tf.onAction = (ae: ActionEvent) => { subfolder.subfolder = tf.text.value} }
  lvp.lvs.getSelectionModel.getSelectedItems.onChange( (aaa,bbb) => {
    // show protocol details
    subfolder=aaa.head
    if (subfolder!=null) {
      server.currentSubFolder = lvp.lvs.items.get().indexOf(subfolder)
      tfName.tf.text = subfolder.name
      tfSubFolder.tf.text = subfolder.subfolder
    } else {
      server.currentSubFolder = -1
      tfName.tf.text = "..."
      tfSubFolder.tf.text = "..."
    }
  })
  top = new Label() { text = "Subfolders:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfSubFolder) }
}


object Main extends JFXApp with Logging {

  // to empty DB
//  new File(DBSettings.dbpath).delete()

  val menu = new Menu("File") {

    items.add(new MenuItem("Open"))
    items.add(new MenuItem("Close"))

  }


  var profile = null

  val menuBar = new MenuBar {
    useSystemMenuBar = true
    minWidth = 100
    menus.add(menu)
  }

  val logView = new TextArea() {
    text = "huhu"
  }
  var spv : SplitPane = null
  var serverView = new ServerView(Store.config) {
    def onServerChange {
      protocolView = new ProtocolView(server)
      subfolderView = new SubFolderView(server)
      spv.items(1) = protocolView
      spv.items(2) = subfolderView
      println("huhuhuhu" + protocolView)
    }
  }
  var protocolView = new ProtocolView(null)
  var subfolderView = new SubFolderView(null)

  spv = new SplitPane {
    orientation = jgo.VERTICAL
    dividerPositions = (0.3)
    items += (serverView, protocolView, subfolderView)//, logView)
  }

  def lookupTextField(id: String) : TextField = {
    val yy = stage.scene.get.lookup(id)
    yy.getClass
    val tf : TextField  = yy.asInstanceOf[javafx.scene.control.TextField]
    tf
  }

  val toolBar = new ToolBar {
    content = List(new Button("sync") {
      onAction = (ae: ActionEvent) => {
//        profileView.profiles.add("asdf")
//        println("tft=" + lookupTextField("#subfolder").text.value)
//        val ppp = new Profile (
//          name = serverView.tfhbServerName.tf.text.value,
//          localFolder = serverView.tfhbLocalFolder.tf.text.value,
//          protocol = new TransferProtocol(
//            name = protocolView.tfProtocolName.tf.text.value,
//            conntype = ConnType.withName(protocolView.cbProtocol.value.get),
//            basefolder = protocolView.tfBaseFolder.tf.text.value
//          ),
//          subfolder = subfolderView.tfSubFolder.text.value
//        )
//        val cl = ppp.synchronize()
      }
    },
    new Button("save settings") {
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

  stage = new Stage{
    title = "CheckBox Test"
    width = 800
    height = 600
    scene = new Scene {
      //      fill = Color.LIGHTGRAY
      content = maincontent
      onCloseRequest =  {
        // TODO: why are these methods called on startup??? disabled for now.
//        Store.save
//        println("close requested" + Store)
      }
    }
  }
  maincontent.prefHeight <== stage.scene.height
  maincontent.prefWidth <== stage.scene.width

  // init
  if (Store.config.currentServer > -1) {
    serverView.onServerChange
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

