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
import sfsync.store.Store
import Helpers._
import sfsync.synchro.CompareFinished

import akka.actor.ActorDSL._
import javafx.util.Callback
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{implicitConversions, reflectiveCalls}
import scalafx.scene.Scene

object CF {
  val amap = Map(A_MERGE -> "M", A_NOTHING -> "==", A_RMLOCAL -> "<-(rm)", A_RMREMOTE -> "(rm)->",
    A_UNKNOWN -> "?", A_USELOCAL -> "->", A_USEREMOTE -> "<-", A_CACHEONLY -> "C", A_RMBOTH -> "<-rm->")
}

// the thing with properties for javafx tableview
class CompFile(cf_ : ComparedFile) {
  val cf = cf_
  var tmp = ""
  if (cf.flocal != null) tmp=cf.flocal.path
  else if (cf.fremote != null) tmp=cf.fremote.path
  else if (cf.fcache != null) tmp=cf.fcache.path
  val path = new StringProperty(this, "path", tmp)
  val status = new StringProperty(this, "status", CF.amap(cf.action))
  val dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  val detailsLocal = new StringProperty(this, "detailsl",
    (if (cf.flocal != null) dformat.format(new java.util.Date(cf.flocal.modTime)) + "," + cf.flocal.size else "none"))
  val detailsRemote = new StringProperty(this, "detailsr",
    (if (cf.fremote != null) dformat.format(new java.util.Date(cf.fremote.modTime)) + "," + cf.fremote.size else "none"))
  def changeAction() {
    status.set(CF.amap(cf.action))
  }
  override def toString: String = "CompFile: " + path() + " " + status() + " L:" + detailsLocal + " R:" + detailsRemote
}
object CompFile

class FilesView() extends Tab {
  text = "Files"
  closable = false

  var comparedfiles = new sfxc.ObservableBuffer[ComparedFile]()
  var compfiles =  new sfxc.ObservableBuffer[CompFile]() // for tableview
  var profile: Profile = null
  def setProfile(profilex: Profile) { profile = profilex }

  val colPath = new TableColumn[CompFile, String]("Path") {
    /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/
  }
  colPath.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.path
  })
  val colStatus = new TableColumn[CompFile, String]("Status") {
    prefWidth=50
    /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/
  }
  colStatus.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = {
      param.getValue.status
    }
  })
  colStatus.setCellFactory(new Callback[jfxsc.TableColumn[CompFile, String],jfxsc.TableCell[CompFile, String]] {
    def call(param: jfxsc.TableColumn[CompFile, String]): jfxsc.TableCell[CompFile, String] = {
      val x = new jfxsc.cell.TextFieldTableCell[CompFile, String]() {
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
  val colDetailsLocal = new TableColumn[CompFile, String]("Local") {prefWidth=200/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colDetailsLocal.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.detailsLocal
  })
  val colDetailsRemote = new TableColumn[CompFile, String]("Remote") {prefWidth=200/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colDetailsRemote.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.detailsRemote
  })

  var tv = new TableView[CompFile](compfiles) { // only string-listview is properly updated!
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

  def updateSorting() {
    tv.delegate.getSortOrder.clear()
    tv.delegate.getSortOrder.add(colPath.delegate)
  }

  val btSync = new Button("Synchronize") {
    onAction = (ae: ActionEvent) => {
      future {
        profile.synchronize(comparedfiles.toList)
      }
      unit()
    }
  }
  btSync.setDisable(true)

  def createActionButton(lab: String, action: Int): Button = {
    new Button(lab) {
      onAction = (ae: ActionEvent) => {
        for (idx <- tv.selectionModel.get().getSelectedItems) {
          idx.cf.action = action
          idx.changeAction()
        }
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
    var canSync = true
    for (cf <- comparedfiles) {
      if (cf.action == A_UNKNOWN) canSync = false
    }
    btSync.setDisable(!canSync)
  }

  def updateActionButtons() {
    println("update action buttons")
    var (allLocalPresenet, oneLocalPresent, allRemotePresent, oneRemotePresent) = (true, false, true, false)
    var allEqual = true
    var legal = true
    var existCheck: (Boolean,Boolean) = null // allow only changing of items where existence is equal
    for (idx <- tv.selectionModel.get().getSelectedItems) {
      val cf = idx.cf
      if (existCheck == null)
        existCheck = (cf.flocal!=null,cf.fremote!=null)
      else if (existCheck != (cf.flocal!=null,cf.fremote!=null)) legal = false
      if (cf.flocal == null) allLocalPresenet = false else oneLocalPresent = true
      if (cf.fremote == null) allRemotePresent = false  else oneRemotePresent = true
      if (!cf.isSynced) allEqual = false
    }
    List(btRmLocal, btUseLocal, btMerge, btNothing, btRmBoth, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(true))
    btNothing.setDisable(false) // this only updates cache with remote file
    if (legal) {
      if (allEqual) btRmBoth.setDisable(false)
      else if ((allLocalPresenet && allRemotePresent)) List(btUseLocal,btUseRemote,btMerge,btRmBoth).foreach(bb=>bb.setDisable(false))
      else if (allLocalPresenet) List(btUseLocal,btRmLocal).foreach(bb => bb.setDisable(false))
      else if (allRemotePresent) List(btUseRemote,btRmRemote).foreach(bb => bb.setDisable(false))
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

  def getFilter(cf: ComparedFile) : Boolean = {
    cFilter.getValue match {
      case F.all => true
      case F.changes => cf.action != A_NOTHING || cf.action == A_UNKNOWN
      case F.problems => cf.action == A_UNKNOWN
      case _ => true
    }
  }
  def updateFilter(filter: String) {
    compfiles.clear()
    comparedfiles.filter( cf => getFilter(cf) ).foreach(cf => compfiles.add(new CompFile(cf)))
    runUI { updateSorting() }
  }

  // receive compared files!
  val act = actor(Main.system)(new Act {
    var doit = true
    become {
      case cf: ComparedFile => {
        runUI {
          comparedfiles.add(cf)
          if (getFilter(cf)) compfiles.add(new CompFile(cf))
        }
      }
      case CompareFinished => {
        runUI { updateSorting() }
        runUI { updateSyncButton() }
        runUI { Main.Status.status.value = "ready" }
      }
      case RemoveCF(cf: ComparedFile) => { // called by synchronize
        runUI {
          comparedfiles.remove(cf)
          compfiles.removeAll(compfiles.filter(p => p.cf == cf))
        }
      }
      case 'done => { // this should be called by myself only
        println("done: sender = " + sender.toString)
        btSync.setDisable(true)
        println("exiting actor cw")
        context.stop(self)
      }
    }
  })

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

