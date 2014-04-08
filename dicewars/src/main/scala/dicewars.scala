package main.scala
import main.scala.controller.DicewarController
import main.scala.view.TUI
import main.scala.util.FileUtil
import java.io.File
import scala.collection.mutable.ArrayBuffer
import main.scala.model.Gamefield

object dicewars {
    def main(args: Array[String]) {
    val controller=new DicewarController()
    val game =  new Gamefield
    game.initMap
    game.mapPosition("basicland")
    val tui= new TUI(controller, game)
    //val gui = new SwingGui(controller)
   while(true)
   {
     tui.mapChoice
     tui.processInputLine(readLine())
   }

     //val file =  new File("C:\\study\\workspace\\dicewars\\Maps\\basicland")
      
     
      
  }
}