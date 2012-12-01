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


//  var ppp = new Profile (
//    name = "testprofile",
//    localFolder = "/tmp/testlocal",
//    protocol = new TransferProtocol (
//      name = "protlocalfolder",
//      conntype = ConnType.Local,
//      basefolder = "/tmp/testremote"
//    ),
//    subfolder = "."
//
//  )
//
//  ppp.synchronize()


}


class MyListView[T](val factory: () => T = null, newitems: sfxc.ObservableBuffer[T], currIdx: Int, val onChange: () => Unit ) extends VBox {
  minHeight=120
  println("lv(" + factory.getClass + "): " + currIdx)
  var lvs = new control.ListView[T]() {
    items = newitems
    minHeight = 100
    selectionModel.get().clearSelection()
    selectionModel.get().select(currIdx)
    println("AAAA")
  }
  lvs.getSelectionModel.getSelectedIndices.onChange( (aaa,bbb) => onChange() )
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
    }
  )
}


class MyTextField(labelText: String, fileChooserMode: Int = 0) extends HBox {
  var tf = new TextField() {
    prefWidth = 500
    text = "..."
  }
  content = List(
    new Label() { text = labelText },
    tf)
  if (fileChooserMode>0) {
    val butt = new Button("file...") {
      onAction = (ae: ActionEvent) => {
        val fileChooser = new FileChooser
        val jf = new java.io.File(tf.text.value)
        if (jf.exists() && jf.canRead) {
          fileChooser.setInitialDirectory(jf)
        } else {
          fileChooser.setInitialDirectory(new java.io.File("/"))
        }
        val res = fileChooser.showOpenDialog(Main.stage)
        if (res != null) tf.text = res.toString
      }
    }
    content.add(butt)
  }
}

abstract class ServerView(val config: Config) extends BorderPane {
  def onServerChange()
  var server: Server = null
  def serverChanged() : Unit = {
    val idx = lvs.lvs.getSelectionModel.getSelectedIndices.head
    server=lvs.lvs.items.get.get(idx)
    config.currentServer = idx//lvs.lvs.items.get().indexOf(server)
    tfName.tf.text = server.name
    tfLocalFolder.tf.text = server.localFolder
    onServerChange()
    println("lvs changed")
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged)
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { server.name = tf.text.value } }
  var tfLocalFolder = new MyTextField("Local folder: ",1) { tf.onAction = (ae: ActionEvent) => { server.localFolder = tf.text.value} }

  top = new Label() { text = "Servers:" }
  left = lvs
  right = new VBox() { content = List(tfName, tfLocalFolder) }
}

class ProtocolView(val server: Server) extends BorderPane {
  var protocol: Protocol = null
  val protocols = if (server!=null) server.protocols else new sfxc.ObservableBuffer[Protocol]
  val currprotocol = if (server!=null) server.currentProtocol else -1
  def protocolChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndices.head
    protocol=protocols(idx)
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
  }
  var lvp = new MyListView[Protocol](() => new Protocol,protocols, currprotocol, () => protocolChanged())
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { protocol.name = tf.text.value} }
  var tfBaseFolder = new MyTextField("Base folder: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocolbasefolder = tf.text.value} }
  var tfURI = new MyTextField("Protocol URI: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocoluri = tf.text.value} }
  top = new Label() { text = "Protocols:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfURI, tfBaseFolder) }
}

class SubFolderView(val server: Server) extends BorderPane {
  var subfolder: SubFolder = null
  val subfolders = if (server!=null) server.subfolders else new sfxc.ObservableBuffer[SubFolder]
  val currsubfolder = if (server != null) server.currentSubFolder else -1
  def subfolderChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndices.head
    subfolder=subfolders(idx)
    if (subfolder!=null) {
      server.currentSubFolder = lvp.lvs.items.get().indexOf(subfolder)
      tfName.tf.text = subfolder.name
      tfSubFolder.tf.text = subfolder.subfolder
    } else {
      server.currentSubFolder = -1
      tfName.tf.text = "..."
      tfSubFolder.tf.text = "..."
    }
  }
  var lvp = new MyListView[SubFolder](() => new SubFolder,subfolders, currsubfolder, () => subfolderChanged())
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { subfolder.name = tf.text.value} }
  var tfSubFolder = new MyTextField("Subfolder: ",5) { tf.onAction = (ae: ActionEvent) => { subfolder.subfolder = tf.text.value} }
  top = new Label() { text = "Subfolders:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfSubFolder) }
}

class CompareWindow(var compfiles: sfxc.ObservableBuffer[ComparedFile]) extends VBox {
  var lv = new ListView[String]()
  var slist = new sfxc.ObservableBuffer[String]()
  compfiles.foreach(cf => slist.add(cf.toString))
  lv.items = slist
  lv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  lv.prefWidth <== this.width
  val bottom = new HBox {
    content = List(
      new Button("Synchronize") {
        onAction = (ae: ActionEvent) => {
          compfiles.foreach(cf => println(cf))
          //            profile.synchronize(compfiles) // TODO test if this is modified
        }
      },
      new Button("uselocal") {
        onAction = (ae: ActionEvent) => {
          val iiis = lv.selectionModel.get().getSelectedIndices
          for (idx <- iiis) {
            var cf = compfiles.get(idx)
            cf.action = cf.A_USELOCAL
            lv.items.get().update(idx,cf.toString)
          }

        }
      },
      new Button("Back") {
        onAction = (ae: ActionEvent) => {
          this.finalize()
          Main.showContent
        }
      }
    )
  }
  content = List(lv,bottom)
}

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
      protocolView = new ProtocolView(server)
      subfolderView = new SubFolderView(server)
      spv.items(1) = protocolView
      spv.items(2) = subfolderView
      if (protocolView.currprotocol > -1) {
        protocolView.protocolChanged()
      }
      if (subfolderView.currsubfolder > -1) {
        subfolderView.subfolderChanged()
      }
      println("huhuhuhu" + protocolView)
    }
  }
  var protocolView = new ProtocolView(null)
  var subfolderView = new SubFolderView(null)

  spv = new SplitPane {
    orientation = jgo.VERTICAL
    dividerPositions = (0.3)
    items += (serverView, protocolView, subfolderView)
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
          name = serverView.tfName.tf.text.value,
          localFolder = serverView.tfLocalFolder.tf.text.value,
          protocol = new TransferProtocol(
            name = protocolView.tfName.tf.text.value,
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

