package sfsync

import sfsync.util.Helpers.MyWorker
import sfsync.util._
import sfsync.store._
import sfsync.util.Helpers._
import sfsync.synchro.{GeneralConnection, MyURI, Profile, VirtualFile}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
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

  def sortit() { obsBuffer.sort((x,y) => x.name.getValueSafe.compareTo(y.name.getValueSafe)<0) }

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


  lvs.getSelectionModel.getSelectedIndices.onChange( (_, _) => {
    val newidx = lvs.getSelectionModel.getSelectedIndex
    if (oldidx != newidx) { // onChange is called 4 times if entry edited (looses selection)
      oldidx = newidx
      onChange()
    }
  })

  def beforeDelete(what: T) = true

  val buttons = new HBox {
    children = List(
      new Button("add") {
        onAction = (_: ActionEvent) => {
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
        onAction = (_: ActionEvent) => {
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

  children = List(lvs, buttons)
}

class MyTextField(labelText: String, val onButtonClick: () => Unit, toolTip: String = "", filter: String = "", canDropFile: Boolean = false) extends HBox {
  alignment = Pos.CenterRight
  val bwidth = if (onButtonClick != null) 60 else 0
  var tf = new TextField() {
    text = ""
    if (toolTip != "") tooltip = new Tooltip { text = toolTip }
    text.onChange({
      if (text.getValueSafe.contains("\\")) text.value = text.getValueSafe.replaceAllLiterally("\\","/")
      if (filter != "") {
        if (!text.getValueSafe.matches(filter)) {
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
          if (event.dragboard.getFiles.length == 1) event.acceptTransferModes(scalafx.scene.input.TransferMode.Copy)
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
  children = List(lb, tf)
  spacing = 10

  if (onButtonClick != null) {
    val butt = new Button("Dir...") {
      onAction = (_: ActionEvent) => {
        onButtonClick()
        unit()
      }
    }
    children.add(butt)
  }
}

abstract class ServerView(val config: Config) extends GridPane with Logging {
  margin = insetsstd
  prefHeight = 130
  def onServerChange()
  var server = new Server
  var sdv: ServerDetailView = _
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
    spacing = 5
    alignment = Pos.CenterRight
    val tfID = new MyTextField("Cache ID: ",null, "just leave it") { tf.text <==> server.id }
    val tfFilter = new MyTextField("Filter: ",null, "regex, e.g., (.*12)|(.*e2)") { tf.text <==> server.filterRegexp }
    val tfLocalFolder = new MyTextField(
      "Local root: ",
      () => fcLocalDir(server.localFolder),
      toolTip = "Local base folder such as '/localdir'",
      filter = directoryFilter,
      canDropFile = true
    ) { tf.text <==> server.localFolder }
    var bClearCache = new Button("Clear cache") {
      onAction = (_: ActionEvent) => { Cache.clearCacheFile(server.id.getValueSafe) }
      tooltip = "Clears the cache database for selected sync location"
    }
    val clist = List(tfLocalFolder,tfFilter,tfID,bClearCache)
    children = clist
  }
  var lvs = new MyListView[Server](() => new Server, config.servers, config.currentServer.value, () => serverChanged()) {
    override def beforeDelete(what: Server) = {
      if (dialogOkCancel("Delete server", "Delete server (only settings & cached data)", "Really delete server " + what)) {
        Cache.clearCacheFile(what.id.getValueSafe)
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
  var protocol: Protocol = _
  var pdv: ProtocolDetailView = _
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
    spacing = 5
    var tfBaseFolder = new MyTextField("Base folder: ", null,
      toolTip = "Remote base directory such as '/remotebasedir or C:/remotebasedir'",
      filter = directoryFilter, canDropFile = true) {
      tf.text <==> protocol.protocolbasefolder
    }
    var tfURI = new MyTextField("Protocol URI: ", null, "file:/// or sftp://user[:password]@host:port", "(file:///)|(sftp://\\S+@\\S+:\\S+)") {
      tf.onAction = (_: ActionEvent) => {
        val uri = new MyURI()
        if (uri.parseString(tf.text.value)) {
          if (uri.password != "" && !uri.password.startsWith("##")) {
            info("encrypt password...")
            val crypto = new JavaCryptoEncryption("DES")
            uri.password = "##" + crypto.encrypt(uri.password)
            tf.text.value = uri.toURIString
          }
        }
      }
      tf.text <==> protocol.protocoluri
    }
    val cbDoSetPermissions = new CheckBox("Set permissions (remote)") {
      tooltip = "Set permissions on files/directories on remote server?"
      selected <==> protocol.doSetPermissions
    }
    val cbSetGroupWrite = new CheckBox("Group write (remote)") {
      tooltip = "Sets the group write flag to this on remote server"
      selected <==> protocol.remGroupWrite
    }
    val cbSetOthersWrite = new CheckBox("Others write (remote)") {
      tooltip = "Sets the others write flag to this on remote server"
      selected <==> protocol.remOthersWrite
    }
    val cbCantSetDate = new CheckBox("Server can't set date (Android)") {
      tooltip = "E.g., un-rooted Android devices can't set file date via sftp, select this and I will keep track of times."
      selected <==> protocol.cantSetDate
    }
    var tfExBefore = new MyTextField("Execute before: ", null, "use '#' to separate args") { tf.text <==> protocol.executeBefore }
    var tfExAfter = new MyTextField("Execute after: ", null, "use '#' to separate args") { tf.text <==> protocol.executeAfter }
    children = List(tfURI, tfBaseFolder,
      new HBox { children = List(cbDoSetPermissions, cbSetGroupWrite, cbSetOthersWrite, cbCantSetDate) }, tfExBefore, tfExAfter)
  }
  var lvp = new MyListView[Protocol](() => new Protocol, server.protocols, server.currentProtocol.value, () => protocolChanged())
  lvp.margin = insetsstd
  add(new Label() { text = "Protocols:" ; style = "-fx-font-weight: bold" }, 0, 0)
  add(lvp, 0, 1)
}

class MyFileChooser(server: Server, protocol: Protocol, localremote: Boolean) extends Logging {

  val ADDTOFOLDERSMODE = 1
  val SELECTMODE = 2
  val profile = new Profile(server, protocol, null)
  var myConn: GeneralConnection = _


  var folders: ObservableBuffer[String] = _
  var folder = new StringProperty("")

  var rootPath = "/" // subfolders (for conn.list) are named "subf/subsubf" while the paths are "/subf/subsubf/" ... confusing

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

  def showSelect(msg: String, foldersProperty: StringProperty) {
    folder = foldersProperty
    showIt(SELECTMODE, msg)
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
          val list = new ArrayBuffer[VirtualFile]
          myConn.list(subfolderpath, server.filterRegexp.getValueSafe, (vf) => list.append(vf), recursive = false)
          list.foreach(vf => {
            val newPath = (rootPath + vf.path).replaceAllLiterally("//","/")
            val newFpti = new FilePathTreeItem(newPath, vf.fileName)
            if (path != newPath) getChildren += newFpti
          })
        }
      }
    })

  }
  private def showIt(mtype: Int, msg: String) {
    var tv: TreeView[String] = null

    profile.taskIni.onFailed = () => {
      debug("failed!!!!!" + profile.taskIni.getException)
      throw profile.taskIni.getException
    }
    profile.taskIni.onCancelled = () => {
      debug("cancelled!!!!!")
    }
    profile.taskIni.onSucceeded = () => {
      runUIwait {
        myConn = if (localremote) profile.local else profile.remote

        val rootNode = new FilePathTreeItem(rootPath, "root")

        val btAddToFolders = new Button("Add selected") {
          onAction = (_: ActionEvent) => {
            for (si <- tv.selectionModel().selectedItems) {
              val sif = si.asInstanceOf[FilePathTreeItem]
              if (sif.isDir) folders.add(sif.path.replaceAll("^/", "").replaceAll("/$", ""))
            }
            print("")
          }
        }
        val btSelect = new Button("Select") {
          onAction = (_: ActionEvent) => {
            val si = tv.selectionModel().selectedItems.head.asInstanceOf[FilePathTreeItem]
            folder.value = si.path.replaceAll("^/","").replaceAll("/$","")
            dstage.close()
          }
        }
        tv = new TreeView[String](rootNode) {
          editable = false
          selectionModel().selectionMode = mtype match {
            case ADDTOFOLDERSMODE => SelectionMode.Multiple
            case SELECTMODE => SelectionMode.Single
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
            children = List(
              mtype match {
                case ADDTOFOLDERSMODE => btAddToFolders
                case SELECTMODE => btSelect
              },
              new Button("Close") {
                onAction = (_: ActionEvent) => { dstage.close() }
              }
            )
          }
        }
        dstage.scene = new Scene {
          content = cont
        }
        cont.prefWidth <== dstage.getScene.width
        cont.prefHeight <== dstage.getScene.height

        dstage.showAndWait()
      }
    }
    MyWorker.runTask(profile.taskIni)
  }
}

class SubFolderView(val mainView: MainView, val server: Server) extends GridPane {
  prefHeight = 150
  margin = insetsstd
  alignment = Pos.CenterRight
  gridLinesVisible = false
  var subfolder: SubFolder= _
  var lvp = new MyListView[SubFolder](() => new SubFolder,server.subfolders, server.currentSubFolder.value, () => subfolderChanged())
  import scalafx.scene.control.Button._
  lvp.buttons.children += new Button("Add <Allfiles>") {
    tooltip = "Adds a subset that will synchronize all files"
    onAction = (_: ActionEvent) => {
      val sf = new SubFolder {
        name.value = "All files"
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

  var sfdv: SubFolderDetailView = _
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
    spacing = 5
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
      debug("ptsd: " + realfile.getCanonicalPath + " " + realfile.exists() + " " + realfile.getPath.startsWith(server.localFolder.getValueSafe))
      if (realfile.exists && realfile.isDirectory && realfile.getPath.startsWith(server.localFolder.getValueSafe)) {
        var sd = realfile.getPath.substring(server.localFolder.getValueSafe.length)
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
          event.acceptTransferModes(scalafx.scene.input.TransferMode.Copy)
        } else {
          event.consume()
        }
      }
    }

    var controls = new HBox {
      children = List(
        new Button("Add (local)") {
          onAction = (_: ActionEvent) => {
            val dc = new MyFileChooser(server, mainView.protocolView.protocol, true)
            dc.showAddToFolders("Select local folder:", subfolder.subfolders)
          }
        },
        new Button("Add (remote)") {
          onAction = (_: ActionEvent) => {
            val dc = new MyFileChooser(server, mainView.protocolView.protocol, false)
            dc.showAddToFolders("Select remote folder:", subfolder.subfolders)
          }
        },
        new Button("Delete") {
          onAction = (_: ActionEvent) => {
            lvs.getItems.remove(lvs.selectionModel().getSelectedItem) // multi selection disabled
            unit()
          }
        }
      )
    }

    children = List(lvs,controls)
  }

}
