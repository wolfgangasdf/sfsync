package sfsync

import sfsync.synchro._
import sfsync.synchro.Actions._
import sfsync.store.{SyncEntry2, Cache, Store}
import sfsync.util.Logging
import sfsync.Helpers._

import scala.language.{implicitConversions, reflectiveCalls, postfixOps}
import scalafx.collections.ObservableBuffer
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.beans.property.StringProperty
import scalafx.scene.control.cell.TextFieldTableCell

import javafx.{util => jfxu}
import javafx.scene.{control => jfxsc}

import diffmatchpatch.diff_match_patch
import java.nio.file.{Path, Files}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer

object CF {
  val amap = Map(
    A_MERGE -> "M",
    A_ISEQUAL -> "==",
    A_RMLOCAL -> "<-(rm)",
    A_RMREMOTE -> "(rm)->",
    A_UNKNOWN -> "?",
    A_USELOCAL -> "->",
    A_USEREMOTE -> "<-",
    A_CACHEONLY -> "C",
    A_RMBOTH -> "<-rm->",
    A_UNCHECKED -> "???",
    A_SYNCERROR -> "SE!",
    A_SKIP -> "skip"
  )
  def stringToAction(actionString: String) = {
    val x = amap.map(_.swap)
    x(actionString)
  }
  def stringToColor(actionString: String) = {
    val cmap = Map( // http://docs.oracle.com/javafx/2/api/javafx/scene/doc-files/cssref.html#typecolor
      A_MERGE -> "salmon",
      A_ISEQUAL -> "white",
      A_RMLOCAL -> "salmon",
      A_RMREMOTE -> "salmon",
      A_UNKNOWN -> "red",
      A_USELOCAL -> "lightgreen",
      A_USEREMOTE -> "lightgreen",
      A_CACHEONLY -> "salmon",
      A_RMBOTH -> "salmon",
      A_UNCHECKED -> "red",
      A_SYNCERROR -> "red",
      A_SKIP -> "salmon"
    )
    val a = stringToAction(actionString)
    cmap(a)
  }
}

class FilesView() extends Tab with Logging {
  text = "Files"
  closable = false

  var profile: Profile = null
  private var syncEnabled = false

  val colPath = new TableColumn[SyncEntry2, String]("Path") {
    cellValueFactory = (xx) => { StringProperty(xx.value.path) }
    cellFactory = (xx) => { // tooltip
    val x = new TextFieldTableCell[SyncEntry2, String] {
        // must set tooltip only after cell created! can't override updateItem, do it here...
        onMouseEntered = (event: scalafx.scene.input.MouseEvent) => {
          if (tooltip.value == null) {
            val se = xx.tableView().getItems.get(tableRow.value.getIndex)
            if (se != null) { // mouseentered also on empty cells!
              tooltip = new Tooltip {
                text = se.toStringNice
                style = "-fx-font-family: \"Courier New\";"
              }
            }
          }
        }
      }
      x
    }
  }

  val colStatus = new TableColumn[SyncEntry2, String]("Status") {
    prefWidth=50
    cellValueFactory = (xx) => {
      StringProperty(xx.value.se.status)
    }
// TODO doesn't work, updateItem not called...
//    cellFactory = (xx) => { // tooltip
//      val x = new TextFieldTableCell[SyncEntry, String] {
//        override def updateItem(f: String, empty: Boolean) {
//          debug("HUHUHUHUHUHUHUHUH")
//        }
//      }
//      x
//    }
  }
  colStatus.setCellFactory(new jfxu.Callback[jfxsc.TableColumn[SyncEntry2, String],jfxsc.TableCell[SyncEntry2, String]] {
    def call(param: jfxsc.TableColumn[SyncEntry2, String]): jfxsc.TableCell[SyncEntry2, String] = {
      val x = new jfxsc.cell.TextFieldTableCell[SyncEntry2, String]() {
        override def updateItem(f: String, empty: Boolean) {
          super.updateItem(f, empty)
          if (!empty)
            setStyle("-fx-background-color: " + CF.stringToColor(f) + ";")
          else
            setStyle("")
        }
      }
      x
    }
  })
  val colDetailsLocal = new TableColumn[SyncEntry2, String]("Local") {
    prefWidth=200
    cellValueFactory = (xx) => {
      StringProperty(xx.value.se.detailsLocal)
    }
  }
  val colDetailsRemote = new TableColumn[SyncEntry2, String]("Remote") {
    prefWidth=200
    cellValueFactory = (xx) => { StringProperty(xx.value.se.detailsRemote) }
  }

  var tv = new TableView[SyncEntry2](Cache.observableList) { // only string-listview is properly updated!
    // columns ++= List(col1) // doesn't work
    delegate.getColumns.addAll(
      colDetailsLocal.delegate, colStatus.delegate, colDetailsRemote.delegate, colPath.delegate
    )
    selectionModel().selectedItems.onChange(
      (ob, _) => {
        if (ob.size > 0) {
          debug("selitemsonchange: ob.size=" + ob.size)
          updateActionButtons()
        }
      }
    )
  }

  def setListItems(x: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry2]) {
    tv.items = x
  }

  def updateSyncEntries() { // doesn't work if session that modified did not commit...
    debug("updateSyncEntries in thread " + Thread.currentThread().getId)
    Cache.initializeSyncEntries(onlyRelevant = true, filterActions = getFilter)

    setListItems(Cache.observableList)
    // workaround for tableview update...
    // javafx bug: http://javafx-jira.kenai.com/browse/RT-22599
    tv.getColumns.get(0).setVisible(false)
    tv.getColumns.get(0).setVisible(true)
  }

  object advActions extends Enumeration {
//    type action = Value
    val debug = Value("Debug info")
    val asRemote = Value("Make local as remote")
    val asLocal = Value("Make remote as local")
  }
  val cbAdvanced = new ComboBox[String] {
    maxWidth = 200
    promptText = "Advanced..."
    items = ObservableBuffer(advActions.values.map(x => x.toString).toList)
    onAction = (ae: ActionEvent) => {
      if (value.value != null) {
        advActions.withName(value.value) match {
          case advActions.debug => debug("SE: " + tv.selectionModel().getSelectedItem)
          case advActions.asRemote =>
            profile.iniLocalAsRemote()
            updateSyncEntries()
            updateSyncButton()
          case advActions.asLocal =>
            profile.iniRemoteAsLocal()
            updateSyncEntries()
            updateSyncButton()
        }
        value = null
//        selectionModel.value.clearSelection() // TODO does not work... bug?
      }
    }
  }

  def readFileToString(fn: Path) = {
    val enc = Files.readAllBytes(fn)
    StandardCharsets.UTF_8.decode(ByteBuffer.wrap(enc)).toString
  }
  val btDiff = new Button("Quick diff") {
    onAction = (ae: ActionEvent) => {
      if (profile != null) if (profile.profileInitialized) {
        val se2 = tv.selectionModel().getSelectedItem
        if (se2 != null) {
          if (se2.se.lSize + se2.se.rSize < 100000) {
            val lf = Files.createTempFile("sfsync-localfile", ".tmp")
            val rf = Files.createTempFile("sfsync-remotefile", ".tmp")
            profile.local.getfile(se2.path, se2.se.lTime, lf.toString)
            profile.remote.getfile(se2.path, se2.se.rTime, rf.toString)
            val lfc = readFileToString(lf)
            val rfc = readFileToString(rf)
            val diff = new diff_match_patch {
              Diff_Timeout = 0
            }
            debug("lfc:\n" + lfc)
            debug("rfc:\n" + rfc)
            val (d, msg) = if (se2.se.action == A_USELOCAL)
              (diff.diff_main(rfc, lfc), "Changes remote -> local:")
            else (diff.diff_main(lfc, rfc), "Changes local -> remote:")
            diff.diff_cleanupSemantic(d)
            val res = diff.diff_prettyHtml(d)
            Main.Dialog.showMessage(msg, res)
          }
        }
      }
      debug("SE: " + tv.selectionModel().getSelectedItem)
    }
  }

  def createActionButton(lab: String, action: Int): Button = {
    new Button(lab) {
      onAction = (ae: ActionEvent) => {
        for (idx <- tv.selectionModel().getSelectedItems) {
          idx.se.action = action
          Cache.update(idx.path, idx.se) // TODO was: , clearCache = false)
        }
        // advance
        tv.selectionModel().clearAndSelect(tv.selectionModel().getSelectedIndices.max+1)

        updateSyncEntries()
        updateSyncButton()
        print("")
      }
    }
  }
  var btUseLocal = createActionButton("Use local",  A_USELOCAL)
  var btUseRemote = createActionButton("Use remote", A_USEREMOTE)
  var btRmLocal = createActionButton("Delete local", A_RMLOCAL)
  var btRmRemote = createActionButton("Delete remote", A_RMREMOTE)
  var btMerge = createActionButton("Merge", A_MERGE)
  var btSkip = createActionButton("Skip", A_SKIP)
  var btRmBoth = createActionButton("Delete both", A_RMBOTH)
  List(btRmLocal, btUseLocal, btMerge, btSkip, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))

  def updateSyncButton(allow: Boolean): Boolean = {
    syncEnabled = allow
    updateSyncButton()
  }
  // returns true if can synchronize
  def updateSyncButton() = {
    debug("update sync button")
    val canSync = if (syncEnabled) Cache.canSync else false
    Main.btSync.setDisable(!canSync)
    canSync
  }

  var enableActions = false

  def updateActionButtons() {
    debug("update action buttons")
    List(btRmLocal, btUseLocal, btMerge, btSkip, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))
    if (enableActions) {
      var allEqual = true
      var allowAction = true
      var legal = true // all have same file exist status
      var existCheck: (Boolean, Boolean) = null // (alllocalexists, allremoteexists)
      for (se2 <- tv.selectionModel().getSelectedItems) {
        if (existCheck == null)
          existCheck = (se2.se.lSize != -1, se2.se.rSize != -1)
        else
          if (existCheck != (se2.se.lSize != -1, se2.se.rSize != -1)) legal = false
        if (!se2.se.isEqual) allEqual = false
        if (se2.se.action == A_UNCHECKED || se2.se.action == A_CACHEONLY) allowAction = false
      }
      if (allowAction) {
        btSkip.setDisable(false)
        if (legal) {
          if (allEqual) {
            if (existCheck == (true,true)) btRmBoth.setDisable(false)
          } else {
            if (existCheck == (true,true)) List(btUseLocal,btUseRemote,btMerge,btRmBoth).foreach(bb=>bb.setDisable(false))
            else if (existCheck == (true,false)) List(btUseLocal,btRmLocal).foreach(bb => bb.setDisable(false))
            else if (existCheck == (false,true)) List(btUseRemote,btRmRemote).foreach(bb => bb.setDisable(false))
          }
        }
      }
    }
  }

  var filterList = new sfxc.ObservableBuffer[String]()
  object F {
    val all="all"; val changes="changes"; val problems="problems"
    def getAll = (all,changes,problems)
  }
  filterList.addAll (F.all,F.changes,F.problems)
  val cFilter = new ComboBox(filterList) {
    selectionModel().select(Store.config.currentFilter)
    onAction = (ae: ActionEvent) => {
      Store.config.currentFilter.value = selectionModel().getSelectedIndex
      updateSyncEntries()
    }
  }

  var bv = new HBox { content = List(cFilter,btRmLocal, btUseLocal, btMerge,
    btSkip, btRmBoth, btUseRemote, btRmRemote, btDiff, cbAdvanced)
  }

  def getFilter = {
    cFilter.getValue match {
      case F.all => ALLACTIONS
      case F.changes => ALLACTIONS.filter( x => x != A_ISEQUAL && x != A_UNCHECKED )
      case F.problems => List(A_UNKNOWN, A_UNCHECKED, A_SKIP, A_CACHEONLY, A_SYNCERROR)
      case _ => ALLACTIONS
    }
  }

  // init
  debug("FilesView() in thread " + Thread.currentThread().getId)
  val vb = new VBox {
    content = List(tv,bv)
  }

  tv.selectionModel().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  colPath.prefWidth <== (vb.width - colStatus.prefWidth-1 - colDetailsLocal.prefWidth - colDetailsRemote.prefWidth)
  tv.prefHeight <== (vb.height - bv.height)
  bv.prefWidth <== vb.width

  content = vb
}

