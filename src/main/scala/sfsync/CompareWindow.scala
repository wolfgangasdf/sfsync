package sfsync

import scalafx.Includes._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import javafx.{util => jfxu}
import javafx.beans.{value => jfxbv}

import javafx.geometry. {Orientation=>jgo}
import javafx.scene.control. {SelectionMode => jscsm}

import scalafx.event.ActionEvent

import synchro._
import javafx.scene. {control => jfxsc}

import scalafx.beans.property.StringProperty
import actors.Actor

class CompFile(cf_ : ComparedFile) {
  val cf = cf_
  var tmp = ""
  if (cf.flocal != null) tmp=cf.flocal.path
  else if (cf.fremote != null) tmp=cf.fremote.path
  val path = new StringProperty(this, "path", tmp)
  val status = new StringProperty(this, "status", cf.action.toString)
  def changeAction() {
    status.set(cf.action.toString)
  }
  override def toString: String = "CompFile: " + path() + " " + status()
}

class CompareWindow() extends VBox with Actor {
  var comparedfiles = new sfxc.ObservableBuffer[ComparedFile]()
  var compfiles =  new sfxc.ObservableBuffer[CompFile]()

//  { // testing
//    val cf = new ComparedFile(new VirtualFile { path = "/asdf" }, new VirtualFile { path="asdf" }, null)
//    comparedfiles.add(cf)
//    compfiles.add(new CompFile(cf))
//  }

  val colPath = new TableColumn[CompFile, String]("Path") { /*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colPath.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.path
  })
  val colStatus = new TableColumn[CompFile, String]("Status") {prefWidth=100/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  colStatus.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[CompFile, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[CompFile, String]) = param.getValue.status
  })

  var tv = new TableView[CompFile](compfiles) { // only string-listview is properly updated!
//    columns ++= List(col1) // doesn't work
    delegate.getColumns.addAll(
      colStatus.delegate, colPath.delegate
    )
  }
  tv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  tv.prefWidth <== this.width
  colPath.prefWidth <== (this.width - colStatus.prefWidth-1)


  val bottom = new HBox {
    content = List(
      new Button("Synchronize") {
        onAction = (ae: ActionEvent) => {
          comparedfiles.foreach(cf => println(cf))
          //            profile.synchronize(compfiles) // TODO
        }
      },
      new Button("uselocal") {
        onAction = (ae: ActionEvent) => {
          val iiis = tv.selectionModel.get().getSelectedItems
          for (idx <- iiis) {
            idx.cf.action = idx.cf.A_USELOCAL // this propagates to comparedfiles!
            idx.changeAction()
          }

        }
      },
      new Button("Back") {
        onAction = (ae: ActionEvent) => {
          this.finalize()
          Main.showContent
        }
      }
    )
  }
  content = List(tv,bottom)


//  for (cf <- comparedfiles) { compfiles.add(new CompFile(cf)) }
  def act() {
    var doit = true
    while (doit) {
      receive {
        case cf: ComparedFile => {
          comparedfiles.add(cf)
          compfiles.add(new CompFile(cf))
          println("added compfile " + cf)
        }
        case CompareFinished => {
          doit = false
          println("comparefinished!")
        }
      }
    }

  }
}

