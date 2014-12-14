package sfsync

import sfsync.util._
import sfsync.store._
import sfsync.Helpers._
import sfsync.Main.Dialog
import sfsync.synchro.{Profile, MyURI}

import scala.collection.JavaConversions._
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.{collections => sfxc}
import scalafx.event.ActionEvent
import scalafx.beans.property.StringProperty
import scalafx.util.StringConverter
import scalafx.geometry.Pos
import scalafx.collections.ObservableBuffer

import java.net.URLDecoder
import java.text.Normalizer
import javafx.{event => jfxe}
import javafx.scene.{control => jfxsc}
import javafx.{stage => jfxs}

object SVHelpers extends Logging {
  def getDroppedFile(file: java.io.File) = {
    // bug in javafx: filename is url-encoded string, .exists() = false
    val path = URLDecoder.decode(file.getPath,"UTF-8")
    debug("getdrfile=" + path)
    new java.io.File(path)
  }
}

class MyListView[T <: ListableThing](
    val factory: () => T = null,
    var obsBuffer: sfxc.ObservableBuffer[T],
    var currIdx: Int,
    val onChange: () => Unit
    ) extends VBox {

  minWidth = 250
  maxWidth = 250

  def sortit() { obsBuffer.sort((x,y) => x.name.compareTo(y.name)<0) }

  sortit()

  var oldidx = -1
  var lvs = new control.ListView[T]() {
    editable = true
    items = obsBuffer
    selectionModel().clearSelection()
    selectionModel().select(currIdx)
  }
  lvs.cellFactory = TextFieldListCell.forListView(new StringConverter[T] {
    def fromString(string: String): T = {
      // this is only called (?) if editing is finished. overwrite old name
      assert(lvs.getSelectionModel.getSelectedItems.size() == 1)
      val xx = lvs.getSelectionModel.getSelectedItem
      xx.name.value = string
      sortit()
      lvs.getSelectionModel.select(xx)
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

  val buttons = new HBox {
    content = List(
      new Button("add") {
        onAction = (ae: ActionEvent) => {
          val newi = factory()
          obsBuffer += newi
          sortit()
          lvs.selectionModel().clearSelection()
          lvs.selectionModel().select(newi)
          onChange()
          unit()
        }
      },
      new Button("delete") {
        onAction = (ae: ActionEvent) => {
          val idx = lvs.selectionModel().getSelectedIndex
          if (idx >= 0) {
            if (beforeDelete(obsBuffer(idx))) {
              obsBuffer.remove(idx)
              lvs.selectionModel().clearSelection()
              lvs.selectionModel().select(0)
              onChange()
            }
          }
          unit()
        }
      }
    )
  }

  content = List(lvs, buttons)
}

class MyTextField(labelText: String, val onButtonClick: () => Unit, toolTip: String = "", filter: String = "", canDropFile: Boolean = false) extends HBox {
  alignment = Pos.CenterRight
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
          text = toJavaPathSeparator(f.getPath)
        }
        event.consume()
      }
      onDragOver = (event: input.DragEvent) => {
        if (event.dragboard.hasFiles) {
          if (event.dragboard.getFiles.length == 1) event.acceptTransferModes(scalafx.scene.input.TransferMode.COPY)
        } else {
          event.consume()
        }
      }
    }
  }
  var lb = new Label() {
    prefWidth = 150
    text = labelText
    alignment = Pos.CenterRight
    labelFor = tf
  }
  lb.hgrow = Priority.Never
  tf.hgrow = Priority.Always
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

abstract class ServerView(val config: Config) extends GridPane with Logging {
  margin = insetsstd
  prefHeight = 130
  gridLinesVisible = false
  def onServerChange()
  var server = new Server
  var sdv: ServerDetailView = null
  def serverChanged() {
    debug("serverChanged!")
    val idx = lvs.lvs.getSelectionModel.getSelectedIndices.head
    if (idx > -1) {
      server=config.servers(idx)
      config.currentServer.set(idx)

      if (sdv != null) children.remove(sdv)
      sdv = new ServerDetailView
      sdv.prefWidth <== (this.prefWidth - lvs.width)
      add(sdv, 1, 0, 1, 2)

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
    if (res != null) prop.value = toJavaPathSeparator(res.toString)
  }

  class ServerDetailView extends VBox {
    margin = insetsstd
    alignment = Pos.CenterRight
    val tfID = new MyTextField("Cache ID: ",null, "just leave it") { tf.text <==> server.id }
    val tfHourOffset = new MyTextField("Time offset (hours): ",null, "for summer/winter time problems", filter = "^[-+]?\\d+$") { tf.text <==> server.hourOffset }
    val tfFilter = new MyTextField("Filter: ",null, "regex, e.g., (.*12)|(.*e2)") { tf.text <==> server.filterRegexp }
    val tfLocalFolder = new MyTextField(
      "Local root: ",
      () => fcLocalDir(server.localFolder),
      toolTip = "Local base folder such as '/localdir'",
      filter = directoryFilter,
      canDropFile = true
    ) { tf.text <==> server.localFolder }
    var bClearCache = new Button("Clear cache") {
      onAction = (ae: ActionEvent) => { Cache.clearCacheFile(server.id) }
      tooltip = "Clears the cache database for selected sync location"
    }
    val clist = List(tfLocalFolder,tfFilter,tfID,tfHourOffset, bClearCache)
    content = clist
    spacing = 5
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer, () => serverChanged()) {
    override def beforeDelete(what: Server) = {
      if (Dialog.showYesNo("Really delete server " + what)) {
        Cache.clearCacheFile(what.id)
        true
      } else false
    }
  }
  lvs.margin = insetsstd
  add(new Label() { text = "Sync locations:" ; style = "-fx-font-weight: bold" }, 0, 0)
  add(lvs, 0, 1)
}

class ProtocolView(val server: Server) extends GridPane with Logging {
  prefHeight = 100
  margin = insetsstd
  alignment = Pos.CenterRight
  gridLinesVisible = false
  var protocol: Protocol = null
  var pdv: ProtocolDetailView = null
  def protocolChanged() {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    if (idx > -1) {
      protocol=server.protocols(idx)
      server.currentProtocol.value = idx
      if (pdv != null) children.remove(pdv)
      pdv = new ProtocolDetailView
      pdv.prefWidth <== (this.prefWidth - lvp.width)
      add(pdv, 1, 0, 1, 2)
    }
  }
  class ProtocolDetailView extends VBox with Logging {
    margin = insetsstd
    var tfBaseFolder = new MyTextField("Base folder: ", null,
      toolTip = "Remote base directory such as '/remotebasedir'",
      filter = directoryFilter, canDropFile = true) { tf.text <==> protocol.protocolbasefolder }
    var tfURI = new MyTextField("Protocol URI: ", null, "file:/// or sftp://user@host:port", "(file:///)|(sftp://\\S+@\\S+:\\S+)") {
      tf.onAction = (ae: ActionEvent) => {
        val uri = new MyURI()
        if (uri.parseString(tf.text.value)) {
          if (!uri.password.startsWith("##")) {
            info("encrypt password...")
            val crypto = new JavaCryptoEncryption("DES")
            uri.password = "##" + crypto.encrypt(uri.password)
            tf.text.value = uri.toURIString
          }
        }
      }
      tf.text <==> protocol.protocoluri
    }
    var tfExBefore = new MyTextField("Execute before: ", null, "use '#' to separate args") { tf.text <==> protocol.executeBefore }
    var tfExAfter = new MyTextField("Execute after: ", null, "use '#' to separate args") { tf.text <==> protocol.executeAfter }
    content = List(tfURI, tfBaseFolder, tfExBefore, tfExAfter)
  }
  var lvp = new MyListView[Protocol](() => new Protocol, server.protocols, server.currentProtocol.value, () => protocolChanged())
  lvp.margin = insetsstd
  add(new Label() { text = "Protocols:" ; style = "-fx-font-weight: bold" }, 0, 0)
  add(lvp, 0, 1)
}

class MyFileChooser(view: FilesView, server: Server, protocol: Protocol, localremote: Boolean) extends Logging {

  val ADDTOFOLDERSMODE = 1
  val SELECTMODE = 2
  val profile = new Profile(view, server, protocol, null)
  debug("init profile...")
  profile.init()
  var folders: ObservableBuffer[String] = null

  debug("init profile done")

  val myConn = if (localremote) profile.local else profile.remote
  var rootPath = "/" // subfolders (for conn.list) are named "subf/subsubf" while the paths are "/subf/subsubf/" ... confusing
  val rootNode = new FilePathTreeItem(rootPath, "root")

  val dstage = new Stage(jfxs.StageStyle.UTILITY) {
    initOwner(Main.stage)
    initModality(jfxs.Modality.APPLICATION_MODAL)
    width = 700
    height = 500
  }

  def showAddToFolders(msg: String, foldersObsBuffer: ObservableBuffer[String]) {
    folders = foldersObsBuffer
    showIt(ADDTOFOLDERSMODE, msg)
  }

  def showSelect(msg: String) : String = {
    val res = showIt(SELECTMODE, msg)
    res
  }

  class FilePathTreeItem(var path: String, var filename: String) extends jfxsc.TreeItem[String](filename) {
    val (_, isDir) = myConn.checkIsDir(path)
    val isDummy = path == "" && filename == "dummy"

    if (isDir) getChildren += new FilePathTreeItem("", "dummy") // add dummy, if expanded will read

    addEventHandler(jfxsc.TreeItem.branchExpandedEvent, new jfxe.EventHandler[jfxsc.TreeItem.TreeModificationEvent[Nothing]]() {
      override def handle(e: jfxsc.TreeItem.TreeModificationEvent[Nothing]) {
        val source = e.getSource.asInstanceOf[FilePathTreeItem]
        debug("EEEEEEEEEEEEEEE branchExpandedEvent in " + path + " isdir=" + isDir + " isexpanded=" + e.getSource.isExpanded + " sclass=" + source.getClass)
        var haveread = false
        if (isDir) if (!getChildren.head.asInstanceOf[FilePathTreeItem].isDummy) haveread = true // important... but why?
        if (isDir && !haveread) {
          getChildren.clear()
          val fixedPath = Normalizer.normalize(path, Normalizer.Form.NFC)
          val strippedPath: String = if (fixedPath == rootPath) "/" else fixedPath.substring(rootPath.length - 1)
          val subfolderpath = strippedPath.replaceAll("^/","").replaceAll("/$","")
          val list = myConn.list(subfolderpath, server.filterRegexp, null, recursive = false, viaActor = false)
          list.foreach(vf => {
            val newPath = (rootPath + vf.path).replaceAllLiterally("//","/")
            val newFpti = new FilePathTreeItem(newPath, vf.fileName)
            if (path != newPath) getChildren += newFpti
          })
        }
      }
    })

  }
  private def showIt(mtype: Int, msg: String) : String  = {
    var res = ""
    var tv: TreeView[String] = null
    val btAddToFolders = new Button("Add selected") {
      onAction = (ae: ActionEvent) => {
        for (si <- tv.selectionModel().selectedItems) {
          val sif = si.asInstanceOf[FilePathTreeItem]
          if (sif.isDir) folders.add(sif.path.replaceAll("^/", "").replaceAll("/$", ""))
        }
        print("")
      }
    }
    val btSelect = new Button("Select") {
      onAction = (ae: ActionEvent) => {
        val si = tv.selectionModel().selectedItems.head.asInstanceOf[FilePathTreeItem]
        res=si.path.replaceAll("^/","").replaceAll("/$","")
        dstage.close()
      }
    }
    tv = new TreeView[String](rootNode) {
      editable = false
      selectionModel().selectionMode = mtype match {
        case ADDTOFOLDERSMODE => SelectionMode.MULTIPLE
        case SELECTMODE => SelectionMode.SINGLE
      }
    }
    rootNode.expanded = true
    val cont = new BorderPane {
      style = "-fx-background-color: lightblue;"
      top = new Label(msg)
      center = tv
      bottom = new HBox {
        margin = insetsstd
        spacing = 5
        alignment = Pos.Center
        content = List(
          mtype match {
            case ADDTOFOLDERSMODE => btAddToFolders
            case SELECTMODE => btSelect
          },
          new Button("Close") {
            onAction = (ae: ActionEvent) => { res=""; dstage.close() }
          }
        )
      }
    }
    dstage.scene = new Scene {
      content = cont
    }
    cont.prefWidth <== dstage.scene.width
    cont.prefHeight <== dstage.scene.height

    dstage.showAndWait()
    res
  }

}

class SubFolderView(val server: Server) extends GridPane {
  prefHeight = 150
  margin = insetsstd
  alignment = Pos.CenterRight
  gridLinesVisible = false
  var subfolder: SubFolder= null
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder.value, () => subfolderChanged())
  import scalafx.scene.control.Button._
  lvp.buttons.content += new Button("Add <Allfiles>") {
    tooltip = "Adds a subset that will synchronize all files"
    onAction = (ae: ActionEvent) => {
      val sf = new SubFolder {
        name = "All files"
        subfolders += ""
      }
      server.subfolders += sf
      unit()
    }
  }

  lvp.margin = insetsstd
  add(new Label() {
    text = "Subsets:"
    style = "-fx-font-weight: bold"
    tooltip =
      """Subsets allow recursive synchronization
        |of only selected folders""".stripMargin
  }, 0, 0)
  add(lvp, 0, 1)

  var sfdv: SubFolderDetailView = null
  def subfolderChanged() {
    val idx = lvp.lvs.getSelectionModel.getSelectedIndex
    if (idx > -1) {
      subfolder=server.subfolders(idx)
      server.currentSubFolder.value = idx

      if (sfdv != null) children.remove(sfdv)
      sfdv =  new SubFolderDetailView
      sfdv.prefWidth <== (this.prefWidth - lvp.width)
      add(sfdv, 1, 0, 1, 2)
    }
  }
  class SubFolderDetailView extends VBox with Logging {
    margin = insetsstd
    def fcSubfolder(basedir: String, inisf: String) = {
      var ressf: String = ""
      val fileChooser = new DirectoryChooser
      val jf = new java.io.File(basedir + inisf)
      if (jf.exists() && jf.canRead) {
        fileChooser.delegate.setInitialDirectory(jf)
      }
      val res = fileChooser.showDialog(Main.stage)
      if (res != null) {
        ressf = PathToSubdir(res)
      }
      ressf
    }

    def PathToSubdir(file: java.io.File) = {
      var ressf = ""
      val realfile = SVHelpers.getDroppedFile(file)
      debug("ptsd: " + realfile.getCanonicalPath + " " + realfile.exists() + " " + realfile.getPath.startsWith(server.localFolder))
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
        event.consume()
      }
      onDragOver = (event: input.DragEvent) => {
        if (event.dragboard.hasFiles) {
          event.acceptTransferModes(scalafx.scene.input.TransferMode.COPY)
        } else {
          event.consume()
        }
      }
    }

    var controls = new HBox {
      content = List(
        new Button("Add (local)") {
          onAction = (ae: ActionEvent) => {
            val dc = new MyFileChooser(Main.filesView, Main.settingsView.serverView.server, Main.settingsView.protocolView.protocol, true)
            dc.showAddToFolders("Select local folder:", subfolder.subfolders)
          }
        },
        new Button("Add (remote)") {
          onAction = (ae: ActionEvent) => {
            val dc = new MyFileChooser(Main.filesView, Main.settingsView.serverView.server, Main.settingsView.protocolView.protocol, false)
            dc.showAddToFolders("Select remote folder:", subfolder.subfolders)
          }
        },
        new Button("Delete") {
          onAction = (ae: ActionEvent) => {
            lvs.getItems.remove(lvs.selectionModel().getSelectedItem) // multi selection disabled
            unit()
          }
        }
      )
    }

    content = List(lvs,controls)
  }

}
