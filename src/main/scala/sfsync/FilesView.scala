package sfsync

import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.beans.property.{IntegerProperty, StringProperty}

import javafx.{util => jfxu}
import javafx.beans.{value => jfxbv}
import javafx.scene. {control => jfxsc}

import sfsync.synchro._
import sfsync.synchro.Actions._
import store.{MySchema, CacheDB, SyncEntry, Store}
import Helpers._

import akka.actor.ActorDSL._
import javafx.util.Callback
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{implicitConversions, reflectiveCalls, postfixOps}
import scalafx.scene.{Node, Scene}

object CF {
  val amap = Map(A_MERGE -> "M", A_NOTHING -> "==", A_RMLOCAL -> "<-(rm)", A_RMREMOTE -> "(rm)->",
    A_UNKNOWN -> "?", A_USELOCAL -> "->", A_USEREMOTE -> "<-", A_CACHEONLY -> "C", A_RMBOTH -> "<-rm->", A_UNCHECKED -> "???")
}

// the thing with properties for javafx tableview
//class CompFile(cf_ : ComparedFile) {
//  val cf = cf_
//  var tmp = ""
//  if (cf.flocal != null) tmp=cf.flocal.path
//  else if (cf.fremote != null) tmp=cf.fremote.path
//  else if (cf.fcache != null) tmp=cf.fcache.path
//  val path = new StringProperty(this, "path", tmp)
//  val status = new StringProperty(this, "status", CF.amap(cf.action))
//  val dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
//  val detailsLocal = new StringProperty(this, "detailsl",
//    (if (cf.flocal != null) dformat.format(new java.util.Date(cf.flocal.modTime)) + "," + cf.flocal.size else "none"))
//  val detailsRemote = new StringProperty(this, "detailsr",
//    (if (cf.fremote != null) dformat.format(new java.util.Date(cf.fremote.modTime)) + "," + cf.fremote.size else "none"))
//  def changeAction() {
//    status.set(CF.amap(cf.action))
//  }
//  override def toString: String = "CompFile: " + path() + " " + status() + " L:" + detailsLocal + " R:" + detailsRemote
//}

class FilesView() extends Tab {
  text = "Files"
  closable = false

  var profile: Profile = null
  def setProfile(profilex: Profile) { profile = profilex }

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
            val xx = CF.amap
            if (f == xx(A_NOTHING)) setStyle("")
            else if (f==xx(A_CACHEONLY) || f==xx(A_MERGE) || f==xx(A_UNKNOWN)) setStyle("-fx-background-color: salmon;")
            else setStyle("-fx-background-color: lightgreen;")
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
          updateActionButtons()
        }
      }
    )
  }

  def setListItems(x: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry]) {
    tv.setItems(x)
  }

  def updateSyncEntries() {
    // store scrollbar pos
    var sbvv = -1.0
    var sbhv = -1.0
    tv.lookupAll("VirtualScrollBar").toArray.foreach (obj => {
      val sb = obj.asInstanceOf[javafx.scene.control.ScrollBar]
      if (sb.orientation.value == javafx.geometry.Orientation.VERTICAL) sbvv = sb.value.value
      if (sb.orientation.value == javafx.geometry.Orientation.HORIZONTAL) sbhv = sb.value.value
    })
    // javafx bug: http://javafx-jira.kenai.com/browse/RT-22599
    CacheDB.invalidateCache()
    CacheDB.updateSyncEntries(true)
    tv.setItems(null)
    tv.layout()
    setListItems(CacheDB.syncEntries)
    // restore scrollbar pos
    tv.lookupAll("VirtualScrollBar").toArray.foreach (obj => {
      val sb = obj.asInstanceOf[javafx.scene.control.ScrollBar]
      if (sb.orientation.value == javafx.geometry.Orientation.VERTICAL) sb.value.set(sbvv)
      if (sb.orientation.value == javafx.geometry.Orientation.HORIZONTAL) sb.value.set(sbhv)
    })
  }

  def updateSorting() {
    // TODO
  }

  val btSync = new Button("Synchronize") {
    onAction = (ae: ActionEvent) => {
      future {
        profile.synchronize()
      }
      unit()
    }
  }
  btSync.setDisable(true)

  def createActionButton(lab: String, action: Int): Button = {
    new Button(lab) {
      onAction = (ae: ActionEvent) => {
        for (idx <- tv.selectionModel.get().getSelectedItems) {
          idx.action = action
          CacheDB.updateSE(idx)
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
  var btNothing = createActionButton("Do nothing", A_NOTHING)
  var btRmBoth = createActionButton("Delete both", A_RMBOTH)
  List(btRmLocal, btUseLocal, btMerge, btNothing, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))

  def updateSyncButton() {
    println("update sync button")
    btSync.setDisable(CacheDB.canSync)
  }

  def updateActionButtons() {
    println("update action buttons")
    // TODO
    var allEqual = true
    var legal = true
    var existCheck: (Boolean, Boolean) = null // (alllocalexists, allremoteexists)
    for (se <- tv.selectionModel.get().getSelectedItems) {
      if (existCheck == null)
        existCheck = (se.lSize != -1, se.rSize != -1)
      else
        if (existCheck != (se.lSize != -1, se.rSize != -1)) legal = false
      if (!se.isEqual) allEqual = false
    }
    List(btRmLocal, btUseLocal, btMerge, btNothing, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))
    btNothing.setDisable(false) // this only updates cache with remote file
    if (legal) {
      if (allEqual) btRmBoth.setDisable(false)
      else if ((existCheck _1) && (existCheck _2)) List(btUseLocal,btUseRemote,btMerge,btRmBoth).foreach(bb=>bb.setDisable(false))
      else if ((existCheck _1)) List(btUseLocal,btRmLocal).foreach(bb => bb.setDisable(false))
      else if ((existCheck _2)) List(btUseRemote,btRmRemote).foreach(bb => bb.setDisable(false))
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
      updateFilter(selectionModel.get().getSelectedItem)
    }
  }

  var bv = new HBox { content = List(cFilter,btSync, btRmLocal, btUseLocal, btMerge, btNothing, btRmBoth, btUseRemote, btRmRemote) }

  def getFilter(se: SyncEntry) : Boolean = {
    cFilter.getValue match {
      case F.all => true
      case F.changes => se.action != A_NOTHING || se.action == A_UNKNOWN
      case F.problems => se.action == A_UNKNOWN
      case _ => true
    }
  }
  def updateFilter(filter: String) {
    // TODO
//    compfiles.clear()
//    comparedfiles.filter( cf => getFilter(cf) ).foreach(cf => compfiles.add(new CompFile(cf)))
//    runUI { updateSorting() }
  }


  // init
  val vb = new VBox {
    content = List(tv,bv)
  }

  tv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  colPath.prefWidth <== (vb.width - colStatus.prefWidth-1 - colDetailsLocal.prefWidth - colDetailsRemote.prefWidth)
  tv.prefHeight <== (vb.height - bv.height -40) // TODO
  bv.prefWidth <== vb.width

  content = vb
}

