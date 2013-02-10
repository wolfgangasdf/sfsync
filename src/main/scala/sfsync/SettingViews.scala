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
import collection.mutable.ArrayBuffer
import sfsync.Main.Dialog
import scalafx.beans.property.StringProperty
import javafx.scene. {input => jfxsi}

class MyListView[T <: ListableThing](val factory: () => T = null, var obsBuffer: ArrayBuffer[T], var currIdx: Int, val onChange: () => Unit ) extends VBox {
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
      if (slist(ii) != obsBuffer(ii).name) obsBuffer(ii).name.set(slist(ii))
    }
  } )

  def beforeDelete(what: T) = true
  def afterCopy(copyidx: Int) = {} // ugly: is called from somewhere

  content = List(
    lvs,
    new HBox {
      content = List(
        new Button("add") {
          onAction = (ae: ActionEvent) => {
            val newi = factory()
            obsBuffer += newi
            slist.add(newi.toString)
            onChange
            getUnit
          }
        },
        new Button("copy") {
          onAction = (ae: ActionEvent) => {
            val idx = lvs.selectionModel.get().getSelectedIndex
            if (idx >= 0) {
              // it is very hard to clone a not-serializable object, so this hack:
              obsBuffer += obsBuffer(idx) // this clones
              Store.save()
              Store.load() // /this clones
              afterCopy(obsBuffer.length - 1)
              Main.refreshContent
              getUnit
            }
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            val idx = lvs.selectionModel.get().getSelectedIndex
            if (idx >= 0) {
              if (beforeDelete(obsBuffer(idx))) {
                obsBuffer.remove(idx)
                slist.remove(idx)
                onChange
              }
            }
            getUnit
          }
        }
      )
    }
  )
}

class MyTextField(labelText: String, val onButtonClick: () => Unit, toolTip: String = "", filter: String = "") extends HBox {
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
        getUnit
      }
    }
    content.add(butt)
  }
}

abstract class ServerView(val config: Config) extends BorderPane {
  prefHeight = 130
  def onServerChange()
  var server = new Server
  def serverChanged : Unit = {
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
  def fcLocalDir(prop: StringProperty) = { // TODO: make dialog modal. fcLocalDir exits immediately
    val fileChooser = new DirectoryChooser
    val jf = new java.io.File(prop.value)
    if (jf.exists() && jf.canRead) {
      // TODO: not working, fixed in 2.2.6: http://javafx-jira.kenai.com/browse/RT-23449
      fileChooser.delegate.setInitialDirectory(jf)
    }
    val res = fileChooser.showDialog(Main.stage)
    if (res != null) prop.value = res.toString
  }

  class ServerDetailView extends VBox {
    val tfID = new MyTextField("Cache ID: ",null, "just leave it") { tf.text <==> server.id }
    val tfFilter = new MyTextField("Filter: ",null, "regex, e.g., (.*12)|(.*e2)") { tf.text <==> server.filterRegexp }
    val tfLocalFolder = new MyTextField("Local folder: ",() => fcLocalDir(server.localFolder), "/localdir","/.*[^/]") { tf.text <==> server.localFolder }
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
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged) {
    override def beforeDelete(what: Server) = {
      if (Dialog.showYesNo("Really delete server " + what)) {
        Cache.clearCache(what.id)
        true
      } else false
    }
    override def afterCopy(copyidx: Int) {
      val s1 = new Server
      Store.config.servers(copyidx).id = s1.id
      Store.config.currentServer.set(copyidx)
    }
  }
  lvs.margin = insetsstd
  top = new Label() { text = "Servers:" }
  left = lvs
}

class ProtocolView(val server: Server) extends BorderPane {
  prefHeight = 100
  var protocol: Protocol = null
  def protocolChanged() : Unit = {
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
    var tfBaseFolder = new MyTextField("Base folder: ", null, "/remotebasedir", "/.*[^/]") { tf.text <==> protocol.protocolbasefolder }
    var tfURI = new MyTextField("Protocol URI: ", null, "file:/// or sftp://user@host:port", "(file:///)|(sftp://\\S+@\\S+:\\S+)") { tf.text <==> protocol.protocoluri }
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
  def subfolderChanged() : Unit = {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    val itm = lvp.lvs.getSelectionModel.getSelectedItem
    if (idx > -1) {
      subfolder=server.subfolders(idx)
      if (!subfolder.name.equals(itm)) subfolder.name.value = itm // then it was edited!
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
      val path = file.toString
      if (file.exists && file.isDirectory && path.startsWith(server.localFolder)) {
        var sd = path.substring(server.localFolder.length)
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
        if (event.dragboard.hasFiles()) {
          event.acceptTransferModes(scalafx.scene.input.TransferMode.COPY);
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
            getUnit
          }
        },
        new Button("delete") {
          onAction = (ae: ActionEvent) => {
            lvs.getItems().removeAll(lvs.selectionModel.get().getSelectedItems)
            getUnit
          }
        }
      )
    }

    content = List(lvs,controls)
    lvs.prefWidth <== this.width
  }

}
