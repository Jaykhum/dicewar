package main.scala
import main.scala.controller.DicewarController
import main.scala.view.TUI
import main.scala.util.FileUtil
import java.io.File
import scala.collection.mutable.ArrayBuffer
import main.scala.model.Gamefield

object dicewars {
    def main(args: Array[String]) {
//    val controller=new DicewarController()
//    val tui=new TUI(controller)
    //val gui = new SwingGui(controller)

    //while(tui.processInputLine(readLine())){}
     //val file =  new File("C:\\study\\workspace\\dicewars\\Maps\\basicland")
      val game =  new Gamefield
      game.mapPosition("basicland")
      for( i <- 0 to 9; j <- 0  to 19){  
        print(game.world(i)(j))
        if(j == 19)
          println()
      }
  }
}