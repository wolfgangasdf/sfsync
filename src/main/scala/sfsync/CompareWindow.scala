package sfsync

import scalafx.Includes._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}

import javafx.geometry. {Orientation=>jgo}
import javafx.scene.control. {SelectionMode => jscsm}

import scalafx.event.ActionEvent

import synchro._

class CompareWindow(var compfiles: sfxc.ObservableBuffer[ComparedFile]) extends VBox {
  var lv = new ListView[String]() // only string-listview is properly updated!
  var slist = new sfxc.ObservableBuffer[String]()
  compfiles.foreach(cf => slist.add(cf.toString))
  lv.items = slist
  lv.selectionModel.get().setSelectionMode(javafx.scene.control.SelectionMode.MULTIPLE)
  lv.prefWidth <== this.width
  val bottom = new HBox {
    content = List(
      new Button("Synchronize") {
        onAction = (ae: ActionEvent) => {
          compfiles.foreach(cf => println(cf))
          //            profile.synchronize(compfiles) // TODO
        }
      },
      new Button("uselocal") {
        onAction = (ae: ActionEvent) => {
          val iiis = lv.selectionModel.get().getSelectedIndices
          for (idx <- iiis) {
            var cf = compfiles.get(idx)
            cf.action = cf.A_USELOCAL
            lv.items.get().update(idx,cf.toString)
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
  content = List(lv,bottom)
}

