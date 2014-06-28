package main.scala
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import main.scala.view.TUI
import main.scala.view.swing.GUI
import controllers.Application

// tests
import main.scala.model.Dice

object dicewars {

      
  def showMenu =
  {
    println("m|menu: menu")
    println("Zeigt das Auswahlmenu an.")
    println("--------------------------------------------------------------------------------")
    println("t|tui: Textual User-Interface")
    println("Runoption mit TUI.")
    println("--------------------------------------------------------------------------------")
    println("g|gui: Graphical User-Interface")
    println("Runoption mit GUI.")
    println("--------------------------------------------------------------------------------")
  }
  
  def main(wui:Application.type) {
      
    val game = new Gamefield
    val tui= new TUI(game)
    val controller = new DicewarController(game)
	wui.game = game
    controller.joinView(tui)
	controller.joinView(wui)
    
    controller.startGame
    

  }
}
   