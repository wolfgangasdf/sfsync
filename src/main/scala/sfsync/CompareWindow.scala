package sfsync

import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import scalafx.Includes._

import javafx.{util => jfxu}
import javafx.beans.{value => jfxbv}
import javafx.scene. {control => jfxsc}
import scalafx.event.ActionEvent

import synchro._
import Actions._

import scalafx.beans.property.StringProperty
import actors.Actor

// the thing with properties for javafx tableview
class CompFile(cf_ : ComparedFile) {
  val cf = cf_
  val amap = Map(A_MERGE -> "M", A_NOTHING -> "==", A_RMLOCAL -> "<-(rm)", A_RMREMOTE -> "(rm)->",
    A_UNKNOWN -> "?", A_USELOCAL -> "->", A_USEREMOTE -> "<-")
  var tmp = ""
  if (cf.flocal != null) tmp=cf.flocal.path
  else tmp=cf.fremote.path
  val path = new StringProperty(this, "path", tmp)
  val status = new StringProperty(this, "status", amap(cf.action))
  val dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  val detailsLocal = new StringProperty(this, "detailsl",
    (if (cf.flocal != null) dformat.format(new java.util.Date(cf.flocal.modTime)) + "," + cf.flocal.size else "none"))
  val detailsRemote = new StringProperty(this, "detailsr",
    (if (cf.fremote != null) dformat.format(new java.util.Date(cf.fremote.modTime)) + "," + cf.fremote.size else "none"))
  def changeAction() {
    status.set(amap(cf.action))
  }
  override def toString: String = "CompFile: " + path() + " " + status() + " L:" + detailsLocal + " R:" + detailsRemote
}

class CompareWindow() extends VBox with Actor {
  var comparedfiles = new sfxc.ObservableBuffer[ComparedFile]()
  var compfiles =  new sfxc.ObservableBuffer[CompFile]()

  val colPath = new TableColumn[CompFile, String]("Path") { /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colPath.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.path
  })
  val colStatus = new TableColumn[CompFile, String]("Status") {prefWidth=50/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colStatus.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.status
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
//    columns ++= List(col1) // doesn't work
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

  val btSync = new Button("Synchronize") {
    onAction = (ae: ActionEvent) => {
    profile.synchronize(compfiles)
    }
  }
  var btBack = new Button("Back") {
    onAction = (ae: ActionEvent) => {
      Main.showContent
    }
  }

  def createActionButton(lab: String, action: Int): Button = {
    new Button(lab) {
      onAction = (ae: ActionEvent) => {
        for (idx <- tv.selectionModel.get().getSelectedItems) {
          idx.cf.action = action
          idx.changeAction()
          updateSyncButton()
        }
      }
    }
  }
  var btUseLocal = createActionButton("Use local",  A_USELOCAL)
  var btUseRemote = createActionButton("Use remote", A_USEREMOTE)
  var btRmLocal = createActionButton("Delete local", A_RMLOCAL)
  var btRmRemote = createActionButton("Delete remote", A_RMREMOTE)
  var btMerge = createActionButton("Merge", A_MERGE)
  var btNothing = createActionButton("Do nothing", A_NOTHING)

  def updateSyncButton() = {
    var canSync = true
    for (cf <- comparedfiles) {
      if (cf.action == A_UNKNOWN) canSync = false
    }
    btSync.setDisable(!canSync)
  }

  def updateActionButtons() {
    var allLocalPresenet = true
    var allRemotePresent = true
    List(btRmLocal, btUseLocal, btMerge, btNothing, btUseRemote, btRmRemote).foreach(bb => bb.setDisable(false))
    for (idx <- tv.selectionModel.get().getSelectedItems) {
      val cf = idx.cf
      if (cf.flocal == null) { allLocalPresenet = false }
      if (cf.fremote == null) { allRemotePresent = false }
    }
    if (!allLocalPresenet) List(btUseLocal,btRmLocal).foreach(bb => bb.setDisable(true))
    if (!allRemotePresent) List(btUseRemote,btRmRemote).foreach(bb => bb.setDisable(true))
    if (!(allLocalPresenet && allRemotePresent)) btMerge.setDisable(true)
  }

  var bv = new HBox { content = List(btSync, btRmLocal, btUseLocal, btMerge, btNothing, btUseRemote, btRmRemote, btBack) }

  content = List(tv,bv)
  tv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  colPath.prefWidth <== (this.width - colStatus.prefWidth-1 - colDetailsLocal.prefWidth - colDetailsRemote.prefWidth)
  tv.prefHeight <== (this.height - bv.prefHeight)

  // receive compared files!
  def act() {
    var doit = true
    loopWhile(doit) {
      receive {
        case cf: ComparedFile => {
          comparedfiles.add(cf)
          compfiles.add(new CompFile(cf))
          println("added compfile " + cf)
        }
        case CompareFinished => {
          doit = false
          println("comparefinished!")
          updateSyncButton()
          exit()
        }
      }
    }

  }
}

