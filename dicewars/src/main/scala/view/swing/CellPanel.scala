package main.scala.view.swing

import scala.swing._
import main.scala.model.Gamefield

class CellPanel(row: Int, column: Int,var game: Gamefield) extends FlowPanel{
  val givenCellColor = new Color(200, 200, 255)
  val cellColor = new Color(224, 224, 255)
  val highlightedCellColor = new Color(192, 255, 192)
  
  //def myCell = game.world(row, column)
  
  val cellButton = new Button
  {
	  text = "1"
      //font = new Font("Veranda",1,22)
	  preferredSize = new Dimension(30,30)
	  opaque = true
	  //background =
  }
  contents += cellButton
  
}