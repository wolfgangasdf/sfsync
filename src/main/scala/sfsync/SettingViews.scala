package sfsync

import scala.collection.JavaConversions._
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
import sfsync.Main.Dialog
import scalafx.beans.property.StringProperty
import synchro.MyURI
import java.net.URLDecoder
import scalafx.util.StringConverter
//import javafx.{util => jfxu, event => jfxe}
//import javafx.scene.{control => jfxsc}

object SVHelpers {
  def getDroppedFile(file: java.io.File) = {
    // bug in javafx: filename is url-encoded string, .exists() = false
    val path = URLDecoder.decode(file.getPath,"UTF-8")
    new java.io.File(path)
  }
}

class MyListView[T <: ListableThing](
    val factory: () => T = null,
    var obsBuffer: sfxc.ObservableBuffer[T],
    var currIdx: Int,
    val onChange: () => Unit
    ) extends VBox {

  def sortit() { obsBuffer.sort((x,y) => x.name.compareTo(y.name)<0) }

  sortit()

  var oldidx = -1
  var lvs = new control.ListView[T]() {
    editable = true
    items = obsBuffer
    selectionModel.get().clearSelection()
    selectionModel.get().select(currIdx)
  }
  lvs.cellFactory = TextFieldListCell.forListView(new StringConverter[T] {
    def fromString(string: String): T = {
      // this is only called (?) if editing is finished. overwrite old name
      val xx = lvs.getSelectionModel.getSelectedItem
      if (xx != null) {
        xx.name.value = string
        sortit()
        lvs.getSelectionModel.select(xx)
      }
      xx
    }
    def toString(t: T): String = t.toString
  })


  lvs.getSelectionModel.getSelectedIndices.onChange( (aaa,bbb) => {
    val newidx = lvs.getSelectionModel.getSelectedIndex
    if (oldidx != newidx) { // onChange is called 4 times if entry edited (looses selection)
      oldidx = newidx
      onChange()
    }
  })

  def beforeDelete(what: T) = true

  content = List(
    lvs,
    new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            val newi = factory()
            obsBuffer += newi
            sortit()
            lvs.selectionModel.get().clearSelection()
            lvs.selectionModel.get().select(newi)
            onChange
            unit()
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            val idx = lvs.selectionModel.get().getSelectedIndex
            if (idx >= 0) {
              if (beforeDelete(obsBuffer(idx))) {
                obsBuffer.remove(idx)
                lvs.selectionModel.get().clearSelection()
                lvs.selectionModel.get().select(0)
                onChange
              }
            }
            unit()
          }
        }
      )
    }
  )
}

class MyTextField(labelText: String, val onButtonClick: () => Unit, toolTip: String = "", filter: String = "", canDropFile: Boolean = false) extends HBox {
  val bwidth = if (onButtonClick != null) 60 else 0
  var tf = new TextField() {
    text = ""
    if (toolTip != "") tooltip = new Tooltip { text = toolTip }
    text.onChange({
      if (filter != "") {
        if (!text.matches(filter)) {
          style = "-fx-background-color: red;"
        } else {
          style = ""//-fx-background-color: red;"
        }
      }
    })
    if (canDropFile) {
      onDragDropped = (event: input.DragEvent) => {
        if (event.dragboard.getFiles.length == 1) {
          val f = SVHelpers.getDroppedFile(event.dragboard.getFiles.head)
          text = f.getPath
        }
        event.consume
      }
      onDragOver = (event: input.DragEvent) => {
        if (event.dragboard.hasFiles) {
          event.acceptTransferModes(scalafx.scene.input.TransferMode.COPY)
        } else {
          event.consume
        }
      }
    }
  }
  var lb = new Label() {
    prefWidth = 200
    text = labelText
//    alignment = javafx.geometry.Pos.CENTER_RIGHT // doesn't work
    delegate.setAlignment(javafx.geometry.Pos.CENTER_RIGHT)
    labelFor = tf
  }
  lb.prefWidth <== this.prefWidth / 3
  tf.prefWidth <== this.prefWidth * 2/3 - bwidth

  content = List(lb, tf)
  spacing = 10

  if (onButtonClick != null) {
    val butt = new Button("Dir...") {
      onAction = (ae: ActionEvent) => {
        onButtonClick()
        unit()
      }
    }
    content.add(butt)
  }
}

abstract class ServerView(val config: Config) extends BorderPane {
  prefHeight = 130
  def onServerChange()
  var server = new Server
  def serverChanged() {
    val idx = lvs.lvs.getSelectionModel.getSelectedIndices.head
    if (idx > -1) {
      server=config.servers(idx)
      config.currentServer.set(idx)

      val sdv = new ServerDetailView
      sdv.prefWidth <== (this.width - lvs.prefWidth)
      right = sdv

      onServerChange()
    }
  }
  def fcLocalDir(prop: StringProperty) {
    val fileChooser = new DirectoryChooser
    val jf = new java.io.File(prop.value)
    if (jf.exists() && jf.canRead) {
      fileChooser.delegate.setInitialDirectory(jf)
    }
    val res = fileChooser.showDialog(Main.stage)
    if (res != null) prop.value = res.toString
  }

  class ServerDetailView extends VBox {
    val tfID = new MyTextField("Cache ID: ",null, "just leave it") { tf.text <==> server.id }
    val tfFilter = new MyTextField("Filter: ",null, "regex, e.g., (.*12)|(.*e2)") { tf.text <==> server.filterRegexp }
    val tfLocalFolder = new MyTextField(
      "Local folder: ",
      () => fcLocalDir(server.localFolder), "/localdir","/.*[^/]",
      canDropFile = true
    ) { tf.text <==> server.localFolder }
    val cbSkipEqualFiles = new CheckBox("Skip equal files") { selected <==> server.skipEqualFiles }
    var bClearCache = new Button("Clear cache") { onAction = (ae: ActionEvent) => { Cache.clearCache(tfID.tf.text.value)} }
    val clist = List(tfLocalFolder,tfFilter,tfID,cbSkipEqualFiles,bClearCache)
    tfLocalFolder.prefWidth <== this.prefWidth
    tfFilter.prefWidth <== this.prefWidth
    tfID.prefWidth <== this.prefWidth
    cbSkipEqualFiles.prefWidth <== this.prefWidth
    bClearCache.prefWidth <== this.prefWidth
    content = clist
    spacing = 5
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged()) {
    override def beforeDelete(what: Server) = {
      if (Dialog.showYesNo("Really delete server " + what)) {
        Cache.clearCache(what.id)
        true
      } else false
    }
  }
  lvs.margin = insetsstd
  top = new Label() { text = "Servers:" }
  left = lvs
}

class ProtocolView(val server: Server) extends BorderPane {
  prefHeight = 100
  var protocol: Protocol = null
  def protocolChanged() {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    if (idx > -1) {
      protocol=server.protocols(idx)
      server.currentProtocol.value = idx
      val pdv = new ProtocolDetailView
      pdv.prefWidth <== (this.width - lvp.prefWidth)
      right = pdv
    }
  }
  class ProtocolDetailView extends VBox {
    var tfBaseFolder = new MyTextField("Base folder: ", null, "/remotebasedir", "/.*[^/]", canDropFile = true) { tf.text <==> protocol.protocolbasefolder }
    var tfURI = new MyTextField("Protocol URI: ", null, "file:/// or sftp://user@host:port", "(file:///)|(sftp://\\S+@\\S+:\\S+)") {
      tf.onAction = (ae: ActionEvent) => {
        val uri = new MyURI()
        if (uri.parseString(tf.text.value)) {
          if (!uri.password.startsWith("##")) {
            println("encrypt password...")
            val crypto = new JavaCryptoEncryption("DES")
            uri.password = "##" + crypto.encrypt(uri.password, "bvfxsdfk")
            tf.text.value = uri.toURIString
          }
        }
      }
      tf.text <==> protocol.protocoluri
    }
    var tfExBefore = new MyTextField("Execute before: ", null, "use '#' to separate args") { tf.text <==> protocol.executeBefore }
    var tfExAfter = new MyTextField("Execute after: ", null, "use '#' to separate args") { tf.text <==> protocol.executeAfter }
    tfBaseFolder.prefWidth <== this.prefWidth
    tfURI.prefWidth <== this.prefWidth
    tfExBefore.prefWidth <== this.prefWidth
    tfExAfter.prefWidth <== this.prefWidth
    content = List(tfURI, tfBaseFolder, tfExBefore, tfExAfter)
  }
  var lvp = new MyListView[Protocol](() => new Protocol,server.protocols, server.currentProtocol.value, () => protocolChanged())
  lvp.margin = insetsstd
  top = new Label() { text = "Protocols:" }
  left = lvp
}

class SubFolderView(val server: Server) extends BorderPane {
  prefHeight = 150
  var subfolder: SubFolder= null
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder.value, () => subfolderChanged())
  lvp.margin = insetsstd
  top = new Label() { text = "Subfolders:" }
  left = lvp
  def subfolderChanged() {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    if (idx > -1) {
      subfolder=server.subfolders(idx)
      server.currentSubFolder.value = idx

      val sfdv =  new SubFolderDetailView
      right = sfdv
      sfdv.prefWidth <== (this.width - lvp.prefWidth)
    }
  }
  class SubFolderDetailView extends VBox {
    margin = insetsstd
    def fcSubfolder(basedir: String, inisf: String) = {
      var ressf: String = ""
      val fileChooser = new DirectoryChooser
      val jf = new java.io.File(basedir + inisf)
      if (jf.exists() && jf.canRead) {
        fileChooser.delegate.setInitialDirectory(jf)
      }
      // TODO: make dialog modal
      val res = fileChooser.showDialog(Main.stage)
      if (res != null) {
        ressf = PathToSubdir(res)
      }
      ressf
    }

    def PathToSubdir(file: java.io.File) = {
      var ressf = ""
      val realfile = SVHelpers.getDroppedFile(file)
      if (realfile.exists && realfile.isDirectory && realfile.getPath.startsWith(server.localFolder)) {
        var sd = realfile.getPath.substring(server.localFolder.length)
        if (sd.startsWith("/")) sd = sd.substring(1)
        ressf = sd
      }
      ressf
    }

    var lvs = new control.ListView[String]() {
      editable = true
      tooltip = "Add or drag'n'drop folders..."
      items = subfolder.subfolders
      cellFactory = TextFieldListCell.forListView()
      onDragDropped = (event: input.DragEvent) => {
        for (f <- event.dragboard.getFiles) {
          val sd = PathToSubdir(f)
          if (sd != "") subfolder.subfolders.add(sd)
        }
        event.consume
      }
      onDragOver = (event: input.DragEvent) => {
        if (event.dragboard.hasFiles) {
          event.acceptTransferModes(scalafx.scene.input.TransferMode.COPY)
        } else {
          event.consume
        }
      }
    }

    var controls = new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            val res = fcSubfolder(server.localFolder, lvs.selectionModel.get().getSelectedItem)
            if (res != "") {
              subfolder.subfolders.add(res)
            }
            unit()
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            lvs.getItems.removeAll(lvs.selectionModel.get().getSelectedItems)
            unit()
          }
        }
      )
    }

    content = List(lvs,controls)
    lvs.prefWidth <== this.width
  }

}
