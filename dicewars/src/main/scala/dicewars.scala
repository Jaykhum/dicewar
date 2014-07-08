package main.scala
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import main.scala.view.TUI
import main.scala.view.swing.GUI



object dicewars {

  
  def main(args: Array[String]) {
      
    val game = new Gamefield
    val tui= new TUI(game)
    val gui = new GUI(game)
    val controller = new DicewarController(game)
    
    controller.joinView(tui)
    controller.joinView(gui)
    
    controller.startGame
	}
}