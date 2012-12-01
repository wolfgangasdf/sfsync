package sfsync

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.collections.ObservableBuffer

import javafx.geometry. {Orientation=>jgo}
import javafx.scene.control. {SelectionMode => jscsm}

import util.Logging
import scalafx.event.ActionEvent
import store._
import java.io.File
import collection.mutable.ArrayBuffer
import scalafx.beans.property.StringProperty

//import store.MyImplicits._
import synchro._
import synchro.ConnType._

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

  class TFHBox(labeltext: String, tftext: String) {
    var afterUpdate: () => Unit = null
    var serverfield: String => Unit = null
    var tf = new TextField() {
      text = tftext
    }
    def getTFHbox = {
      val res = new HBox { content = List(new Label {text = labeltext}, tf)}
      tf.onAction = { (ae: ActionEvent) => {
        println("tfhbox changed!" + labeltext)
        serverfield(tf.text.value)
        Store.save
        afterUpdate()
      }}
      res
    }
    // give ref like server.name_= (no spaces in between, this is the getter method name!)
    def setServerField(sf: String => Unit, text: String, afterUpdate: () => Unit = () => {}) {
      println("setServerField " + text)
      this.afterUpdate = afterUpdate
      serverfield = sf
      tf.text = text
    }
  }

  class MyListView[T](val factory: () => T = null, val onUpdate: () => Unit, what: String) {
    var obsList  = new ObservableBuffer[T] // local cache needed for ListView
    var listview = new control.ListView[T]() {
      items = obsList
      selectionModel().setSelectionMode(jscsm.SINGLE)
      selectionModel().getSelectedItems.onChange(
        (aaa,bbb) => {
          currIdx = arrayBuf.indexOf(aaa.head)
          currIdxField(currIdx)
          println("list: getSelOnChange!" + currIdx)
          onUpdate()
        }
      )
    }
    var currIdxField: Int => Unit = null
    var currIdx: Int = -1
    var arrayBuf: ArrayBuffer[T] = null // This works, it's an object...

    def getContent = {
      new VBox() {
        content = List(
          listview,
          new HBox {
            content = List(
              new Button("add " + what) {
                onAction = (ae: ActionEvent) => {
                  val snew = factory()
                  arrayBuf += snew
                  Store.save
                  updateList(1)
                  println("added " + what)
                }
              },
              new Button("delete " + what) {
                onAction = (ae: ActionEvent) => {
                  arrayBuf.remove(currIdx)
                  currIdx -= 1
                  currIdxField(currIdx)
                  Store.save
                  updateList(1)
                  println("deleted " + what)
                }
              }
            )
          })
      }
    }
    def setData(currIdxField: Int => Unit, currIdx: Int, arrayBuf: ArrayBuffer[T]) {
      this.arrayBuf = arrayBuf
      this.currIdxField = currIdxField
      this.currIdx = currIdx
    }
    def updateList(reload: Int = 0) = {
      println("----------update! reload=" + reload)
      if (reload==1) { // elements changed!
        obsList.clear()
        if (arrayBuf != null) arrayBuf.foreach(ss => obsList += ss)
        if (currIdx > -1) {
          listview.selectionModel.select(obsList.get(currIdx).toString)
        }
      } else { // element only changed...
//        obsList.set(currIdx, arrayBuf(currIdx))
        obsList.update(currIdx, arrayBuf(currIdx))
        println("info idx="+currIdx + " ab=" + arrayBuf + " ol=" + obsList)
      }

//      listview.selectionModel().clearAndSelect(currIdx)
      println("----------/update idx=" + currIdx)
    }
  }

  val serverView = new BorderPane() {
    var server: Server = null // the current server, also db4o object!
    var tfhbServerName = new TFHBox("Name: ", "...")
    var tfhbLocalFolder = new TFHBox("Local folder: ", "...")
    var lbStatusCache = new Label {text = "Status of cache file"}

    def showServer(): Unit = {
      println("showserver: " + Store.config.currentServer)
      if (-1 < Store.config.currentServer && lvs != null) {
        server = Store.config.servers(Store.config.currentServer)
        tfhbLocalFolder.setServerField(server.localFolder_=, server.localFolder)
        tfhbServerName.setServerField(server.name_=, server.name, () => lvs.updateList(0))
      }
      protocolView.showProtocols(server)
    }

    var lvs = new MyListView[Server](() => new Server,this.showServer, "Server")
    lvs.setData(Store.config.currentServer_=, Store.config.currentServer, Store.config.servers)
    left = lvs.getContent

    right = new VBox() {
      content = List(
        tfhbServerName.getTFHbox,
        tfhbLocalFolder.getTFHbox,
        new HBox { content = List(lbStatusCache)}
      )
    }
    // initialize
    lvs.updateList(1)
  }

  val protocolView = new BorderPane() {
    var server: Server = null // current server, db4o!
    var protocol: Protocol = null // current protocol, db4o object!
    var connTypeList = new ObservableBuffer[String]()
    ConnType.values.foreach(ctv => connTypeList.add(ctv.toString))//("local", "sftp")

    var tfProtocolName = new TFHBox("protocolname: ","...")
    var tfProtocol = new TFHBox("Protocol: ", "...")
    var cbProtocol = new ComboBox(connTypeList)
//    var tfServerURL = new TFHBox("Server URL: ", "...")
//    var tfPassword = new TFHBox("Password: ", "...")
    var tfBaseFolder = new TFHBox("Base Folder: ", "...")

    def showProtocol(): Unit = {
      if (-1 < server.currentProtocol && lvp != null) {
        protocol = server.protocols(server.currentProtocol)
        tfProtocolName.setServerField(protocol.name_=, protocol.name,() => lvp.updateList(0))
        tfBaseFolder.setServerField(protocol.basefolder_=, protocol.basefolder)
        tfProtocol.setServerField(protocol.protocol_=, protocol.protocol)
      }
    }
    var lvp = new MyListView[Protocol](() => new Protocol,this.showProtocol, "Protocol")
    def showProtocols(server: Server) = {
      this.server = server
      lvp.setData(server.currentProtocol_=, server.currentProtocol, server.protocols)
      lvp.updateList(1) // initialize
    }

    left = lvp.getContent
    right = new VBox() {
      content = List(
        tfProtocolName.getTFHbox,
        tfProtocol.getTFHbox,
        tfBaseFolder.getTFHbox,
        new HBox{ content = List(new Label {text = "Connection Type: "}, cbProtocol)}
      )
    }
  }

  val subfolderView = new BorderPane() {

    var tfSubFolderName = new TextField() {id="subfoldername"; text = "All"}
    var tfSubFolder = new TextField() {id="subfolder"; text = "."}

    left = new ListView[String]() {

    }
    right = new VBox() {
      content = List(
        new HBox{ content = List(new Label {text = "Name: "}, tfSubFolderName)},
        new HBox{ content = List(new Label {text = "Subfolder: "}, tfSubFolder)}
      )
    }
  }

  val logView = new TextArea() {
    text = "huhu"
  }


  val spv = new SplitPane {
    orientation = jgo.VERTICAL
    dividerPositions = (0.3)
    items += (serverView, protocolView, subfolderView, logView)
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
        val ppp = new Profile (
          name = serverView.tfhbServerName.tf.text.value,
          localFolder = serverView.tfhbLocalFolder.tf.text.value,
          protocol = new TransferProtocol(
            name = protocolView.tfProtocolName.tf.text.value,
            conntype = ConnType.withName(protocolView.cbProtocol.value.get),
            basefolder = protocolView.tfBaseFolder.tf.text.value
          ),
          subfolder = subfolderView.tfSubFolder.text.value
        )
        val cl = ppp.synchronize()
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
//        Store.dumpConfig
//        Store.save
        println("close requested" + Store)
      }
    }
  }
  maincontent.prefHeight <== stage.scene.height
  maincontent.prefWidth <== stage.scene.width

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

