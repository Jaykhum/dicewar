package main.scala
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import main.scala.view.TUI
import main.scala.view.swing.GUI


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
  
  def main(args: Array[String]) {
      
    val game = new Gamefield
    val tui= new TUI(game)
    val gui = new GUI(game)
    val controller = new DicewarController(game)
    
    controller.joinView(tui)
    controller.joinView(gui)
    
    controller.startGame
    
//    var run = true
//    showMenu
//    while(run)
//    {
//      println("Bitte waehlen Sie eine UserInterface-Variante aus!")
//
//      readLine match
//      {
//        	case ("m"|"menu") => showMenu
//			case ("t"| "tui") => controller.joinView(tui); run = false	
////			case ("g"| "gui") => controller.joinView(gui); run = false
//			//case ("w"| "wui") =>
//			case ("q"| "quit") => run = false
//			case _ => println("Fehlerhafte Eingabe, bitte wiederholen!")
//      }
//    }
  }
}
    //controller.joinView(tui)
    //controller.joinView(gui)
   
      
     /*
     * Test
     * */
/*
     var attackDice = new Array[Int](10)
      var dice = new Dice
	  for(i <- 0 to attackDice.length -1)
	  {
	       attackDice(i) = dice.roll
	       println(i + ": " + attackDice(i))
	  }
 */
     
      
      
//    val game =  new Gamefield
//    game.initMap
//    val tui= new TUI(controller, game)
//    tui.showMenu
//	while(true)
//	{
//		tui.processInputLine(readLine())
//	}


     //val file =  new File("C:\\study\\workspace\\dicewar\\dicewars\\Maps\\")
    
//    var player1 = new Avatar
//    player1.setId(1)
//    game.mapPosition("basicland");
//    
//    game.tactic(player1, game.world(4)(9), game.world(4)(10))
//    game.tactic(player1, game.world(4)(10), game.world(4)(11))
//    game.tactic(player1, game.world(4)(9), game.world(5)(9))
//    game.tactic(player1, game.world(3)(5), game.world(4)(9))
//    game.tactic(player1, game.world(7)(15), game.world(4)(9))

    // linker n
//    print("4,10 und 4,9:  ")
//    game.singleAttack(game.world(4)(10), game.world(4)(9))
//    // rechts
//    print("4,10 und 4,11:  ")
//    game.singleAttack(game.world(4)(10), game.world(4)(11))
//    
//    print("4,5 und 4,9:  ")
//    game.singleAttack(game.world(4)(5), game.world(4)(9))
//    print("3,5 und 4,9:  ")
//    game.singleAttack(game.world(3)(5), game.world(4)(9))
//    // unten n
//    print("3,5 und 4,5:  ")
//    game.singleAttack(game.world(3)(5), game.world(4)(5))
//    // oben n
//    print("5,5 und 4,5:  ")
//    game.singleAttack(game.world(5)(5), game.world(4)(5))
//    var p1 = new Avatar
//    p1.setId(1)
//    
//    game.reinforcement(p1)
//    tui.showTUI
//  }
//}
