package main.scala
import main.scala.controller.DicewarController
import main.scala.view.TUI
import main.scala.util.FileUtil
import java.io.File
import scala.collection.mutable.ArrayBuffer
import main.scala.model.Gamefield
import main.scala.model.Avatar

object dicewars {
    def main(args: Array[String]) {
    val controller=new DicewarController()
    val game =  new Gamefield
    game.initMap
    val tui= new TUI(controller, game)
    tui.showMenu
//   while(true)
//   {
//     tui.mapChoice
//     tui.processInputLine(readLine())
//   }


     //val file =  new File("C:\\study\\workspace\\dicewar\\dicewars\\Maps\\")
    
    /*
     * Test
     * */
    // linker n
    print("4,10 und 4,9:  ")
    game.battle(game.world(4)(10), game.world(4)(9))
    // rechts
    print("4,10 und 4,11:  ")
    game.battle(game.world(4)(10), game.world(4)(11))
    
    print("4,5 und 4,9:  ")
    game.battle(game.world(4)(5), game.world(4)(9))
    print("3,5 und 4,9:  ")
    game.battle(game.world(3)(5), game.world(4)(9))
    // unten n
    print("3,5 und 4,5:  ")
    game.battle(game.world(3)(5), game.world(4)(5))
    // oben n
    print("5,5 und 4,5:  ")
    game.battle(game.world(5)(5), game.world(4)(5))
    var p1 = new Avatar
    p1.setId(1)
    
    game.reinforcement(p1)
    tui.showTUI
  }
}
