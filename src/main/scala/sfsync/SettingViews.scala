package sfsync

import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}

import javafx.geometry. {Orientation=>jgo}
import javafx.scene.control. {SelectionMode => jscsm}

import scalafx.event.ActionEvent
import sfsync.store._

class MyListView[T](val factory: () => T = null, var obsBuffer: sfxc.ObservableBuffer[T], var currIdx: Int, val onChange: () => Unit ) extends VBox {
  minHeight=120
  var slist = new sfxc.ObservableBuffer[String]()
  println("create lv(" + factory.getClass + "): " + currIdx)
  obsBuffer.foreach(cf => slist.add(cf.toString))
  var lvs = new control.ListView[String]() {
    items = slist
    minHeight = 100
    selectionModel.get().clearSelection()
    selectionModel.get().select(currIdx)
  }
  def updateItem(idx: Int) {
    var it = obsBuffer.get(idx)
    lvs.items.get().update(idx,it.toString)
  }
  lvs.getSelectionModel.getSelectedIndices.onChange( (aaa,bbb) => onChange() )
  content = List(
    lvs,
    new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            val newi = factory()
            obsBuffer.add(newi)
            slist.add(newi.toString)
            println("asdf")
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            val idx = lvs.selectionModel.get().getSelectedIndex
            obsBuffer.remove(idx)
            lvs.items.get().remove(idx)
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
  content = List(new Label() { text = labelText }, tf)
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
    println("************ serverChanged: idx=" + idx)
    server=config.servers.get(idx)
    config.currentServer = idx//lvs.lvs.items.get().indexOf(server)
    tfName.tf.text = server.name
    tfLocalFolder.tf.text = server.localFolder
    println("*********** currprot=" + server.currentProtocol)
    onServerChange()
    println("*********** /serverChanged currserv=" + config.currentServer + " currprot=" + server.currentProtocol)
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged)
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { server.name = tf.text.value }}//; lvs.updateItem(config.currentServer) } }
  var tfLocalFolder = new MyTextField("Local folder: ",1) { tf.onAction = (ae: ActionEvent) => { server.localFolder = tf.text.value} }

  top = new Label() { text = "Servers:" }
  left = lvs
  right = new VBox() { content = List(tfName, tfLocalFolder) }
}

class ProtocolView(val server: Server) extends BorderPane {
  var protocol: Protocol = null
  println("protocolView: " + server.currentProtocol)
  def protocolChanged() : Unit = {
    println("protocolChanged")
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    protocol=server.protocols(idx)
    println("protocol=" + protocol)
    server.currentProtocol = idx
    tfName.tf.text = protocol.name
    tfBaseFolder.tf.text = protocol.protocolbasefolder
    tfURI.tf.text = protocol.protocoluri
  }
  var lvp = new MyListView[Protocol](() => new Protocol,server.protocols, server.currentProtocol, () => protocolChanged())
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { protocol.name = tf.text.value} }
  var tfBaseFolder = new MyTextField("Base folder: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocolbasefolder = tf.text.value} }
  var tfURI = new MyTextField("Protocol URI: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocoluri = tf.text.value} }
  top = new Label() { text = "Protocols:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfURI, tfBaseFolder) }
}

class SubFolderView(val server: Server) extends BorderPane {
  var subfolder: SubFolder= null
  val currsubfolder = if (server != null) server.currentSubFolder else -1
  def subfolderChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    subfolder=server.subfolders(idx)
    server.currentSubFolder = idx
    tfName.tf.text = subfolder.name
    tfSubFolder.tf.text = subfolder.subfolder
  }
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, currsubfolder, () => subfolderChanged())
  var tfName = new MyTextField("Name: ") { tf.onAction = (ae: ActionEvent) => { subfolder.name = tf.text.value } }
  var tfSubFolder = new MyTextField("Subfolder: ",5) { tf.onAction = (ae: ActionEvent) => { subfolder.subfolder = tf.text.value} }
  top = new Label() { text = "Subfolders:" }
  left = lvp
  right = new VBox() { content = List(tfName, tfSubFolder) }
}
