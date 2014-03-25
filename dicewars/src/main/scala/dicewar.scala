package scala

import main.scala.controller.DicewarController
import main.scala.view.TUI

object dicewar {
    def main(args: Array[String]) {
    val controller=new DicewarController()
    val tui=new TUI(controller)
    //val gui = new SwingGui(controller)

    //while(tui.processInputLine(readLine())){}
  }
}