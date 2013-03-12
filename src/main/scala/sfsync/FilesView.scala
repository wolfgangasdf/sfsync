package sfsync

import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.beans.property.StringProperty

import javafx.{util => jfxu}
import javafx.beans.{value => jfxbv}
import javafx.scene. {control => jfxsc}

import sfsync.synchro._
import sfsync.synchro.Actions._
import store.{CacheDB, SyncEntry, Store}
import Helpers._

import javafx.util.Callback
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{implicitConversions, reflectiveCalls, postfixOps}
import javafx.collections.ObservableList

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

class FilesView() extends Tab {
  text = "Files"
  closable = false

  var profile: Profile = null
  private var syncEnabled = false

//  val colSelected = new TableColumn[SyncEntry, String]("Sel") {
//    /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/
//  }
//  colSelected.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[SyncEntry, String], jfxbv.ObservableValue[String]] {
//    def call(param: jfxsc.TableColumn.CellDataFeatures[SyncEntry, String]) = StringProperty(if (param.getValue.selected) "x" else "")
//  })
  val colPath = new TableColumn[SyncEntry, String]("Path") {
    /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/
  }
  colPath.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[SyncEntry, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[SyncEntry, String]) = StringProperty(param.getValue.path)
  })
  val colStatus = new TableColumn[SyncEntry, String]("Status") {
    prefWidth=50
    /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/
  }
  colStatus.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[SyncEntry, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[SyncEntry, String]) = param.getValue.status
  })
  colStatus.setCellFactory(new Callback[jfxsc.TableColumn[SyncEntry, String],jfxsc.TableCell[SyncEntry, String]] {
    def call(param: jfxsc.TableColumn[SyncEntry, String]): jfxsc.TableCell[SyncEntry, String] = {
      val x = new jfxsc.cell.TextFieldTableCell[SyncEntry, String]() {
        override def updateItem(f: String, empty: Boolean) {
          if (!empty) {
            super.updateItem(f, empty)
            setStyle("-fx-background-color: " + CF.stringToColor(f) + ";")
          }
        }
      }
      x
    }
  })
  val colDetailsLocal = new TableColumn[SyncEntry, String]("Local") {prefWidth=200/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colDetailsLocal.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[SyncEntry, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[SyncEntry, String]) = { param.getValue.detailsLocal }
  })
  val colDetailsRemote = new TableColumn[SyncEntry, String]("Remote") {prefWidth=200/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colDetailsRemote.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[SyncEntry, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[SyncEntry, String]) = param.getValue.detailsRemote
  })

  var tv = new TableView[SyncEntry](CacheDB.syncEntries) { // only string-listview is properly updated!
    // columns ++= List(col1) // doesn't work
    delegate.getColumns.addAll(
      colDetailsLocal.delegate, colStatus.delegate, colDetailsRemote.delegate, colPath.delegate
    )
    selectionModel.get().selectedItems.onChange(
      (ob, _) => {
        if (ob.size > 0) {
          println("selitemsonchange: ob.size=" + ob.size)
          updateActionButtons()
        }
      }
    )
  }

  def setListItems(x: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry]) {
    tv.setItems(x)
  }

  def updateSyncEntries() {
    println("updateSyncEntries in thread " + Thread.currentThread().getId)
    CacheDB.invalidateCache()
    CacheDB.updateSyncEntries(Option(true), getFilter)


    setListItems(CacheDB.syncEntries)
    // workaround for tableview update...
    // javafx bug: http://javafx-jira.kenai.com/browse/RT-22599
    tv.getColumns().get(0).setVisible(false);
    tv.getColumns().get(0).setVisible(true);
  }

  val btDebugInfo = new Button("Debug info") {
    onAction = (ae: ActionEvent) => {
      println("SE: " + tv.selectionModel.get().getSelectedItem)
    }
  }

  def createActionButton(lab: String, action: Int): Button = {
    new Button(lab) {
      onAction = (ae: ActionEvent) => {
        for (idx <- tv.selectionModel.get().getSelectedItems) {
          idx.action = action
          CacheDB.updateSE(idx, false)
        }
        updateSyncEntries()
        updateSyncButton()
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

  def updateSyncButton(allow: Boolean) {
    syncEnabled = allow
    updateSyncButton()
  }
  def updateSyncButton() {
    println("update sync button")
    if (syncEnabled)
      Main.btSync.setDisable(!CacheDB.canSync)
    else
      Main.btSync.setDisable(true)
  }

  var enableActions = false

  def updateActionButtons() {
    println("update action buttons")
    List(btRmLocal, btUseLocal, btMerge, btSkip, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))
    if (enableActions) {
      var allEqual = true
      var allowAction = true
      var legal = true // all have same file exist status
      var existCheck: (Boolean, Boolean) = null // (alllocalexists, allremoteexists)
      for (se <- tv.selectionModel.get().getSelectedItems) {
        if (existCheck == null)
          existCheck = (se.lSize != -1, se.rSize != -1)
        else
          if (existCheck != (se.lSize != -1, se.rSize != -1)) legal = false
        if (!se.isEqual) allEqual = false
        if (se.action == A_UNCHECKED || se.action == A_CACHEONLY) allowAction = false
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
    selectionModel.get().select(Store.config.currentFilter)
    onAction = (ae: ActionEvent) => {
      Store.config.currentFilter.value = selectionModel.get().getSelectedIndex
      updateSyncEntries()
    }
  }

  var bv = new HBox { content = List(cFilter,btRmLocal, btUseLocal, btMerge, btSkip, btRmBoth, btUseRemote, btRmRemote, btDebugInfo) }

  def getFilter = {
    cFilter.getValue match {
      case F.all => ALLACTIONS
      case F.changes => ALLACTIONS.filter( x => ( x != A_ISEQUAL && x != A_UNCHECKED) )
      case F.problems => List(A_UNKNOWN, A_UNCHECKED, A_SKIP, A_CACHEONLY, A_SYNCERROR)
      case _ => ALLACTIONS
    }
  }

  // init
  println("FilesView() in thread " + Thread.currentThread().getId)
  val vb = new VBox {
    content = List(tv,bv)
  }

  tv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  colPath.prefWidth <== (vb.width - colStatus.prefWidth-1 - colDetailsLocal.prefWidth - colDetailsRemote.prefWidth)
  tv.prefHeight <== (vb.height - bv.height)
  bv.prefWidth <== vb.width

  content = vb
}

