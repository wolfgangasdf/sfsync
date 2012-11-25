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
      basefolder = "/tmp/testremote",
      username = "",
      password = ""
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

  val serverView = new BorderPane() {
    var serverList  = new ObservableBuffer[Server]
    var server: Server = null // the current server
    var tfServerName = new TextField() {
      id="servername"; text = "server1"
    }
    var tfLocalFolder = new TextField() {id="localfolder"; text = "/tmp/testlocal"}
    var lbStatusCache = new Label {text = "Status of cache file"}
    var lvServers = new control.ListView[Server]() {
      items = serverList
      selectionModel().setSelectionMode(jscsm.SINGLE)
      selectionModel().getSelectedItems.onChange(
        (aaa,bbb) => {
          val newidx = Store.config.servers.indexOf(aaa.head)
          if (Store.config.currentServer != newidx) { // avoid loop
            Store.config.currentServer = newidx
            showServer
          }
          println("aaa=" + aaa + "," + bbb + " new index = " + Store.config.currentServer)
        }
      )
    }

    def updateList {
      println("----------update!")
      serverList.clear()
      Store.config.servers.foreach(ss => serverList += ss)
      println("----------/update len=" + Store.config.servers.length)
    }

    def showServer {
      if (-1 < Store.config.currentServer) {
        server = Store.config.servers(Store.config.currentServer)
        tfServerName.text = server.name
        tfLocalFolder.text = server.localFolder
      }
//      protocolView.showProtocols(newserver.protocols.toList,newserver.currentProtocol)
    }

    left = new VBox() {
      content = List(
        lvServers,
      new HBox {
        content = List(
          new Button("add server") {
            onAction = (ae: ActionEvent) => {
              val snew = new Server
              Store.config.servers += snew
              Store.save
              updateList
              println("added server")
            }
          },
          new Button("delete server") {
            onAction = (ae: ActionEvent) => {
              Store.config.servers.remove(Store.config.currentServer)
              Store.config.currentServer = -1
              Store.save
              updateList
              println("deleted")
            }
          }
        )
      })
    }
    right = new VBox() {
      content = List(
        new HBox { content = List(new Label {text = "Name:"}, tfServerName)},
        new HBox { content = List(new Label {text = "Local base folder:"}, tfLocalFolder)},
        new HBox { content = List(lbStatusCache)}
      )
    }
    // initialize
    updateList
    if (-1 < Store.config.currentServer) {
      lvServers.selectionModel().clearAndSelect(Store.config.currentServer)
      showServer
    }

    // add events, must be after init???????
    tfServerName.onAction = { (ae: ActionEvent) => {
      server.name = tfServerName.text.value
      Store.save
      updateList
      println("event: " + ae.eventType + " : " + ae)
    }}
  }

  val protocolView = new BorderPane() {
    var protocols: List[Protocol] = null
    var protocolsList = new ObservableBuffer[String]()
    var connTypeList = new ObservableBuffer[String]()
    ConnType.values.foreach(ctv => connTypeList.add(ctv.toString))//("local", "sftp")

    var tfProtocolName = new TextField() {id="protocolname"; text = "proto1"}
    var tfProtocol = new TextField() {id="protocol"; text = "local"}
    var cbProtocol = new ComboBox(connTypeList)
    var tfUserName = new TextField
    var tfPassword = new TextField
    var tfBaseFolder = new TextField() {id="basefolder"; text = "/tmp/testremote"}

    def showProtocols(newprotocols: List[Protocol], currProt: Int) {
      protocols = newprotocols
      // TODO
    }
    left = new ListView[String]() {
      items = protocolsList
      selectionModel().setSelectionMode(jscsm.SINGLE)
    }
    right = new VBox() {
      content = List(
        new HBox { content = List(new Label {text = "Name:"}, tfProtocolName)},
        new HBox{ content = List(new Label {text = "Connection Type: "}, cbProtocol)},
        new HBox { content = List(new Label {text = "Remote base folder:"}, tfBaseFolder)},
        new HBox { content = List(new Label {text = "Username:"}, tfUserName, new Label {text = "Password:"}, tfPassword)}
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
          name = serverView.tfServerName.text.value,
          localFolder = serverView.tfLocalFolder.text.value,
          protocol = new TransferProtocol(
            name = protocolView.tfProtocolName.text.value,
            conntype = ConnType.withName(protocolView.cbProtocol.value.get),
            basefolder = protocolView.tfBaseFolder.text.value,
            username = protocolView.tfUserName.text.value,
            password = protocolView.tfPassword.text.value
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
//      content = mainContent
      onCloseRequest =  {
        Store.dumpConfig
        Store.save
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

//  debug("load plugins...")
//  // get access to loaded scene! HOW? I think only via javafx....
//  // is scalafx a one-way street?
//  // RIGHT NOW: nearly no scalafx, could remove it.
//  val scontent = mycontent.getChildren.asScala
//  scontent.foreach(ccc => println(ccc.getClass + ": " + ccc))

}

