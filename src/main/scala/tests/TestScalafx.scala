package tests

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import paint.Color
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent


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
