package tests

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.scene.paint.Color
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import javafx.util
import javafx.scene.control
import javafx.beans.value
import scalafx.scene
import scalafx.beans.property.StringProperty
import javafx.collections.ListChangeListener
import collection.mutable.{ListBuffer, ArrayBuffer}
import java.util


object TestSFXScenes extends JFXApp {
  class MainScene extends Scene {
    fill = Color.LIGHTGREEN
    content = new VBox {
      content = List(
        new Button("button") {
          onAction = (ae: ActionEvent) => {
            println("button main!")
          }
        },
        new Button("show other scene") {
          onAction = (ae: ActionEvent) => {
            println("show other!")
            setScene(otherScene)
          }
        }
      )
    }
  }
  class OtherScene extends Scene {
    fill = Color.LIGHTCORAL
    content = new VBox {
      content = List(
        new Button("button") {
          onAction = (ae: ActionEvent) => println("button otherscene!")
        },
        new Button("close") {
          onAction = (ae: ActionEvent) => {
            println("close!")
            setScene(mainScene)
          }
        }
      )
    }
  }
  val mainScene = new MainScene
  val otherScene = new OtherScene
  stage = new JFXApp.PrimaryStage {
    title = "Hello Stage"
    width = 600
    height = 450
    scene = mainScene
  }
  def setScene(newscene: Scene) {
    stage.scene = newscene
  }
}


import javafx.{util => jfxu}
import javafx.scene.{control => jfxsc}
import javafx.beans.{value => jfxbv}
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx. {collections => sfxc}
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.beans.property.StringProperty

object TestSFXTableView extends JFXApp {
  class MyItem(var name: StringProperty)

  val col1 = new TableColumn[MyItem, String]("Name") {prefWidth=200/*cellValueFactory = _.value.firstName// DOESNT WORK do below*/ }
  col1.setCellValueFactory(new jfxu.Callback[jfxsc.TableColumn.CellDataFeatures[MyItem, String], jfxbv.ObservableValue[String]] {
    def call(param: jfxsc.TableColumn.CellDataFeatures[MyItem, String]) = { param.getValue.name }
  })

  var listeners = new ListBuffer[ListChangeListener[_ >: MyItem]]()

  var database = new ListBuffer[String]()
  for (i <- 1 to 50) database += i.toString

  var itemList =  new com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[MyItem]() {
    def get(p1: Int): MyItem = {
      println("get " + p1)
      return new MyItem(StringProperty(database(p1)))
    }
    def size(): Int = 50

    override def addListener(p1: ListChangeListener[_ >: MyItem]) {
      println("adding listener " + p1)
      listeners += p1
      super.addListener(p1)
    }
  }


  var tv = new TableView[MyItem](itemList) {
    delegate.getColumns.addAll(col1)
  }

  import scala.collection.JavaConversions._
  stage = new JFXApp.PrimaryStage {
    title = "Hello Stage"
    width = 600
    height = 450
    scene = new Scene {
      content = new VBox {
        content = List(
          new HBox {
            content = List(
              new Button("change selected item") {
                onAction = (ae: ActionEvent) => {
                  val si = tv.selectionModel.get().getSelectedItem
                  val sidx = tv.selectionModel.get().getSelectedIndex
                  println("change item " + database(sidx))
                  database(sidx) += "y"
                  si.name.set(si.name.value + "x")
                  val obc = new sfxc.ObservableBuffer[MyItem]()
                  obc.add(si)
                  val ch = new ListChangeListener.Change[MyItem](obc) {
                    def next(): Boolean = false

                    def reset() {}

                    def getFrom: Int = sidx

                    def getTo: Int = sidx

                    def getRemoved: java.util.List[MyItem] = List()

                    def getPermutation: Array[Int] = Array()

                    override def wasUpdated(): Boolean = true
                  }
                  listeners.foreach(l => l.onChanged(ch))
                }
              },
              new Button("change item 45") {
                onAction = (ae: ActionEvent) => {
                  val sidx = 45
                  println("change item " + database(sidx))
                  database(sidx) += "y"
                  val obc = new sfxc.ObservableBuffer[MyItem]()
                  val ch = new ListChangeListener.Change[MyItem](obc) {
                    def next(): Boolean = false

                    def reset() {}

                    def getFrom: Int = sidx

                    def getTo: Int = sidx

                    def getRemoved: java.util.List[MyItem] = List()

                    def getPermutation: Array[Int] = Array()

                    override def wasUpdated(): Boolean = true
                  }
                  listeners.foreach(l => l.onChanged(ch))
                }
              },
              new Button("close") {
                onAction = (ae: ActionEvent) => {
                  println("close!")

                }
              }
            )
          },
        tv
        )
      }
    }
  }
}
