package sfsync

import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import cell.TextFieldListCell
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
    editable = true
    items = slist
    cellFactory = TextFieldListCell.forListView()

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
    val itm = lvs.lvs.getSelectionModel.getSelectedItem
    if (idx > -1) {
      server=config.servers.get(idx)
      if (!server.name.equals(itm)) server.name = itm // then it was edited!
      config.currentServer = idx//lvs.lvs.items.get().indexOf(server)
      tfLocalFolder.tf.text = server.localFolder
      tfID.tf.text = server.id
      onServerChange()
    }
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged)
  var tfLocalFolder = new MyTextField("Local folder: ",1) { tf.onAction = (ae: ActionEvent) => { server.localFolder = tf.text.value} }
  var tfID = new MyTextField("Server ID (for cache): ",1) { tf.onAction = (ae: ActionEvent) => { server.id = tf.text.value} }

  top = new Label() { text = "Servers:" }
  left = lvs
  right = new VBox() { content = List(tfLocalFolder,tfID) }
}

class ProtocolView(val server: Server) extends BorderPane {
  var protocol: Protocol = null
  def protocolChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    val itm = lvp.lvs.getSelectionModel.getSelectedItem
    protocol=server.protocols(idx)
    if (!protocol.name.equals(itm)) protocol.name = itm // then it was edited!
    server.currentProtocol = idx
    tfBaseFolder.tf.text = protocol.protocolbasefolder
    tfURI.tf.text = protocol.protocoluri
  }
  var lvp = new MyListView[Protocol](() => new Protocol,server.protocols, server.currentProtocol, () => protocolChanged())
  var tfBaseFolder = new MyTextField("Base folder: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocolbasefolder = tf.text.value} }
  var tfURI = new MyTextField("Protocol URI: ") { tf.onAction = (ae: ActionEvent) => { protocol.protocoluri = tf.text.value} }
  top = new Label() { text = "Protocols:" }
  left = lvp
  right = new VBox() { content = List(tfURI, tfBaseFolder) }
}

class SubFolderView(val server: Server) extends BorderPane {
  var subfolder: SubFolder= null
  def subfolderChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    val itm = lvp.lvs.getSelectionModel.getSelectedItem
    subfolder=server.subfolders(idx)
    if (!subfolder.name.equals(itm)) subfolder.name = itm // then it was edited!
    server.currentSubFolder = idx
    tfSubFolder.tf.text = subfolder.subfolder
  }
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder, () => subfolderChanged())
  var tfSubFolder = new MyTextField("Subfolder: ",5) { tf.onAction = (ae: ActionEvent) => { subfolder.subfolder = tf.text.value} }
  top = new Label() { text = "Subfolders:" }
  left = lvp
  right = new VBox() { content = List(tfSubFolder) }
}
