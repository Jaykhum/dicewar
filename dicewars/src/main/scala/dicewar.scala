package scala

import main.scala.controller.DicewarController
import main.scala.view.TUI
import main.scala.util.FileUtil
import java.io.File

object dicewar {
    def main(args: Array[String]) {
//    val controller=new DicewarController()
//    val tui=new TUI(controller)
    //val gui = new SwingGui(controller)

    //while(tui.processInputLine(readLine())){}
     val fu = new FileUtil
     //val file =  new File("C:\\study\\workspace\\dicewars\\Maps\\basicland")
     fu.readData("C:\\study\\workspace\\dicewars\\Maps\\basicland")
  }
}