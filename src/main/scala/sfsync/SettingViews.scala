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
import scala._

class MyListView[T <: ListableThing](val factory: () => T = null, var obsBuffer: sfxc.ObservableBuffer[T], var currIdx: Int, val onChange: () => Unit ) extends VBox {
  var oldidx = -1
  var slist = new sfxc.ObservableBuffer[String]()
  obsBuffer.foreach(cf => slist.add(cf.toString))
  var lvs = new control.ListView[String]() {
    editable = true
    items = slist
    cellFactory = TextFieldListCell.forListView()

//    prefHeight = 200
    selectionModel.get().clearSelection()
    selectionModel.get().select(currIdx)
  }

  lvs.getSelectionModel().getSelectedIndices.onChange( (aaa,bbb) => {
    val newidx = lvs.getSelectionModel.getSelectedIndex
    if (oldidx != newidx) { // onChange is called 4 times if entry edited (looses selection)
      oldidx = newidx
      onChange()
    }
  } )

  slist.onChange( { // list has been edited: update obsBuffer... ugly
    for (ii <- 0 until slist.length) {
      if (slist(ii) != obsBuffer(ii).name) obsBuffer(ii).name = slist(ii)
    }
  } )

  content = List(
    lvs,
    new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            val newi = factory()
            obsBuffer.add(newi)
            slist.add(newi.toString)
            onChange
            print("")
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            val idx = lvs.selectionModel.get().getSelectedIndex
            obsBuffer.remove(idx)
            slist.remove(idx)
            onChange
            print("")
          }
        }
      )
    }
  )
}


class MyTextField(labelText: String, fileChooserMode: Int = 0, toolTip: String = "") extends HBox {
  var tf = new TextField() {
    prefWidth = 500
    text = ""
    if (toolTip != "") tooltip = new Tooltip { text = toolTip }
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
      }
    }
    content.add(butt)
  }
}

abstract class ServerView(val config: Config) extends BorderPane {
  minHeight = 160
  def onServerChange()
  var server = new Server
  def serverChanged() : Unit = {
    val idx = lvs.lvs.getSelectionModel.getSelectedIndices.head
    if (idx > -1) {
      server=config.servers.get(idx)
      config.currentServer.set(idx)

      right = new ServerDetailView

      onServerChange()
    }
  }
  class ServerDetailView extends VBox {
    val tfID = new MyTextField("Cache ID: ",0, "just leave it") { tf.text <==> server.id }
    val tfFilter = new MyTextField("Filter: ",0, "e.g., (.*12)|(.*e2)") { tf.text <==> server.filterRegexp }
    val tfLocalFolder = new MyTextField("Local folder: ",1) { tf.text <==> server.localFolder }
    val cbSkipEqualFiles = new CheckBox("Skip equal files") { selected <==> server.skipEqualFiles }
    var bClearCache = new Button("Clear cache") { onAction = (ae: ActionEvent) => { Cache.clearCache(tfID.tf.text.value)} }
    content = List(tfLocalFolder,tfFilter,tfID,cbSkipEqualFiles,bClearCache)
    spacing = 5
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged)
  top = new Label() { text = "Servers:" }
  left = lvs
}

class ProtocolView(val server: Server) extends BorderPane {
  minHeight = 160
  var protocol: Protocol = null
  def protocolChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    if (idx > -1) {
      protocol=server.protocols(idx)
      server.currentProtocol = idx
      right = new ProtocolDetailView
    }
  }
  class ProtocolDetailView extends VBox {
    var tfBaseFolder = new MyTextField("Base folder: ") { tf.text <==> protocol.protocolbasefolder }
    var tfURI = new MyTextField("Protocol URI: ") { tf.text <==> protocol.protocoluri }
    var tfExBefore = new MyTextField("Execute before: ") { tf.text <==> protocol.executeBefore }
    var tfExAfter = new MyTextField("Execute after: ") { tf.text <==> protocol.executeAfter }
    content = List(tfURI, tfBaseFolder, tfExBefore, tfExAfter)
  }
  var lvp = new MyListView[Protocol](() => new Protocol,server.protocols, server.currentProtocol.value, () => protocolChanged())
  top = new Label() { text = "Protocols:" }
  left = lvp
}

class SubFolderView(val server: Server) extends BorderPane {
  minHeight = 150
  var subfolder: SubFolder= null
  def subfolderChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    val itm = lvp.lvs.getSelectionModel.getSelectedItem
    if (idx > -1) {
      subfolder=server.subfolders(idx)
      if (!subfolder.name.equals(itm)) subfolder.name = itm // then it was edited!
      server.currentSubFolder = idx

      right = new SubFolderDetailView
    }
  }
  class SubFolderDetailView extends VBox {
    var tfSubFolder = new MyTextField("Subfolder: ",5) { tf.text <==> subfolder.subfolder }
    content = List(tfSubFolder)
  }
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder.value, () => subfolderChanged())
  top = new Label() { text = "Subfolders:" }
  left = lvp
}
