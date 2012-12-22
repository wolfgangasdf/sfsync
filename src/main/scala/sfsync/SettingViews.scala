package sfsync

import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import cell.TextFieldListCell
import scalafx. {collections => sfxc}
import scalafx.event.ActionEvent
import sfsync.store._
import sfsync.Helpers._

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


class MyTextField(labelText: String, fileChooserMode: Int = 0, toolTip: String = "") extends HBox {
  var tf = new TextField() {
    prefWidth = 500
    text = "..."
    tooltip = new Tooltip { text = toolTip }
  }
  var lb = new Label() {
    prefWidth = 200
    text = labelText
//    alignment = javafx.geometry.Pos.CENTER_RIGHT // doesn't work
    delegate.setAlignment(javafx.geometry.Pos.CENTER_RIGHT)
    labelFor = tf
  }
  content = List(lb, tf)
  spacing = 10

  if (fileChooserMode>0) {
    val butt = new Button("Dir...") {
      onAction = (ae: ActionEvent) => {
        val fileChooser = new DirectoryChooser
        val jf = new java.io.File(tf.text.value)
        if (jf.exists() && jf.canRead) {
          fileChooser.setInitialDirectory(jf)
        } else {
          fileChooser.setInitialDirectory(new java.io.File("/"))
        }
        runUI { // TODO: make dialog modal
          val res = fileChooser.showDialog(Main.stage)
          if (res != null) tf.text = res.toString
        }
        println("finished")
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
      tfFilter.tf.text = server.filterRegexp
      onServerChange()
    }
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged)
  var tfLocalFolder = new MyTextField("Local folder: ",1) { tf.onAction = (ae: ActionEvent) => { server.localFolder = tf.text.value} }
  var tfID = new MyTextField("Cache ID: ",1, "just leave it") { tf.onAction = (ae: ActionEvent) => { server.id = tf.text.value} }
  var tfFilter = new MyTextField("Filter: ",1, "e.g., (.*12)|(.*e2)") { tf.onAction = (ae: ActionEvent) => { server.filterRegexp = tf.text.value} }
  var bClearCache = new Button("Clear cache") { onAction = (ae: ActionEvent) => { Cache.clearCache(tfID.tf.text.value)} }

  top = new Label() { text = "Servers:" }
  left = lvs
  right = new VBox() { content = List(tfLocalFolder,tfFilter,tfID,bClearCache) }
}

class ProtocolView(val server: Server) extends BorderPane {
  var protocol: Protocol = null
  def protocolChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    val itm = lvp.lvs.getSelectionModel.getSelectedItem
    if (idx > -1) {
      println("idx=" + idx + " sp" + server.protocols)
      protocol=server.protocols(idx)
      if (!protocol.name.equals(itm)) protocol.name = itm // then it was edited!
      server.currentProtocol = idx
      tfBaseFolder.tf.text = protocol.protocolbasefolder
      tfURI.tf.text = protocol.protocoluri
    }
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
    if (idx > -1) {
      subfolder=server.subfolders(idx)
      if (!subfolder.name.equals(itm)) subfolder.name = itm // then it was edited!
      server.currentSubFolder = idx
      tfSubFolder.tf.text = subfolder.subfolder
    }
  }
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder, () => subfolderChanged())
  var tfSubFolder = new MyTextField("Subfolder: ",5) { tf.onAction = (ae: ActionEvent) => { subfolder.subfolder = tf.text.value} }
  top = new Label() { text = "Subfolders:" }
  left = lvp
  right = new VBox() { content = List(tfSubFolder) }
}
