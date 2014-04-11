package main.scala.view
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import scala.io.Source._
import swing._
import com.sun.xml.internal.fastinfoset.algorithm.HexadecimalEncodingAlgorithm

class TUI (var controller: DicewarController, var game: Gamefield) extends Reactor {
  
   val delimiterVertical = "|";
   val delimiterHorizontal: Char = '-';
   val labelHorizontal = Array[String] ("  A ", "  B ", "  C ", "  D ", "  E ", "  F ", "  G ", "  H ", "  I ", "  J ", "  K ", "  L ", "  M ", "  N ", "  O ", "  P ", "  Q ", "  R ")
   val labelVertical = Array[String] ("01", "02", "03", "04", "05", "06", "07", "08", "09","10")
  
   
   def showTUI = {
     // label top
     print("    ")
     for(h <- 0 to game.width-1)
     {
       print(labelHorizontal(h))
     }
     println()
     // trenner oben
     print("    ")
     for(i <- 4 to 80-4)
     {
       print(delimiterHorizontal)
     }
     print("    ")
     println()
     // field
     
      for( j <- 0 to game.height-1; k <- 0  to game.width+1){

        if(k == 0)
        {
          print(" "+ labelVertical(j) + delimiterVertical)
        }else if(k == 19)
        {
          print(delimiterVertical + labelVertical(j) + " ")
          println()
          helpLimiter
        }else
        {
        print(delimiterVertical + game.world(j)(k-1).showImage + delimiterVertical)
        }
        
      }     
      // Label bottom
       print("    ")
     for(h <- 0 to game.width-1)
     {
       print(labelHorizontal(h))
     }
     println()
   }
   
   def processInputLine(input:String)
   {
     //Todo
	 showTUI
   }
   
   def helpLimiter() 
   {
     print("    ")
      for(i <- 4 to 80-4)
     {
       print("-")
     }
     print("    ")
     println()
   }
   
   def mapChoice = 
   {
     printM4
   }

   def printM1 = 
   {
     println("Basicland")
     println("------------")
     println("|          |")  
     println("|   $$$$   |")
     println("|    $$$$  |")
     println("|          |")
     println("------------")
   }
   
   def printM2 = 
   {
     println("Land 2")
     println("------------")
     println("|       $$$|")  
     println("|     $$$$$|")
     println("|   $$$$$$$|")
     println("|$$$$$$$$$$|")
     println("------------")
   }
   
   def printM3 = 
   {
     println("Land 3")
     println("------------")
     println("|  $$$$$$$$|")  
     println("| $$$   $$$|")
     println("|$$   $$$$ |")
     println("|$$$$$$$$  |")
     println("------------")
   }
   
   def printM4 = 
   {
     println("Land 4")
     println("------------")
     println("|$$$$$$$$$$|")  
     println("|  $$  $$$$|")
     println("|  $$  $$$$|")
     println("|$$$$$$$$$$|")
     println("------------")
   }
   
   
}