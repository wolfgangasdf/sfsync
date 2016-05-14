package sfsync

import scalafx.geometry.Insets
import scalafx.scene.control.{Label, Tab, TextArea}
import scalafx.scene.layout.BorderPane

class LogView extends Tab {
  text = "Log"

  val taLog = new TextArea()

  content = new BorderPane {
    margin = Insets(5.0)
    top = new Label("Log view, also logged to file!")
    center = taLog
  }

}
