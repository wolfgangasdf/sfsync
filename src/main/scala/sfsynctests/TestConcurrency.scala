package sfsynctests

import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene._
import scalafx.stage._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.event.ActionEvent
import scalafx. {collections => sfxc}
import javafx. {stage => jfxs}
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global

object TestConcurrency extends JFXApp {
  var obslist = new sfxc.ObservableBuffer[String]
  obslist += "asdf1"
  obslist += "asdf2"
  def runUI( f: => Unit ) {
    javafx.application.Platform.runLater( new Runnable() {
      def run() {
        f
      }
    })
  }
  def runUIwait( f: => Any ) : Any = {
    var stat: Any = null
//    synchronized(stat) // not needed??
    val runable = new Runnable() {
      def run() {
        stat = f
        println("after f!!!!!")
      }
    }
    javafx.application.Platform.runLater(runable)
    while(stat == null) {
      Thread.sleep(1)
    }
    println("after loopwhile!!!!!")
    stat
  }
  stage = new JFXApp.PrimaryStage {
    title = "SFSynchro"
    width = 800
    height = 600
    scene = new Scene {
      //      fill = Color.LIGHTGRAY
      content = new BorderPane {
        center = new ListView[String] {
          items = obslist
        }
        top = new Button("add directly") {
          onAction = (ae: ActionEvent) => { obslist += "new!!!" ; println("added") }
        }
        bottom = new Button("add many in other thread") {
          onAction = (ae: ActionEvent) => {
            future {
              while (true) {
                Thread.sleep(1000)
                println("xxx")
                val res = runUIwait( Dialog.showTest() )
                runUI { obslist += "newt" + res ; println("nt") }
                if (res == true) println("tr") else println("fa")

                println("yyy")
              }
            }
            println("here at end") }
        }
      }
    }
  }



  object Dialog {
    val dstage = new Stage(jfxs.StageStyle.UTILITY) {
      initOwner(stage) // TODO remove
      initModality(jfxs.Modality.APPLICATION_MODAL)
      width = 500
      height = 300
    }
    def showTest() : Boolean = {
      var res = -1
      dstage.scene = new Scene {
        content = new BorderPane {
          center = new Label { text = "huhu" }
          bottom = new HBox {
            content = List(
              new Button("Ye") { onAction = (ae: ActionEvent) => { res=1; dstage.close } },
              new Button("NO") { onAction = (ae: ActionEvent) => { res=0; dstage.close } }
            )
          }
        }
      }
      dstage.showAndWait
      res==1
    }
  }


}

