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
     showMenu
	   input match
	   {
		  case "1" => mapProcess
		  case "2" => helpView
		  case "Basicland" => gameProcess("basicland")
		  case "Land 2" => gameProcess("land2")
		  case "Land 3" => gameProcess("land3")
		  case "Land 4" => gameProcess("land4")
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen")
	   }
     
   }
   def mapProcess()
   {
     
     var isInputCorrect=false
      while(!isInputCorrect)
     {
       showAllMap 
       isInputCorrect = mapProcessInputLine(readLine())
     }
   }
   
   def mapProcessInputLine(input:String) : Boolean =
   {
	   input match
	   {
		  case "Basicland" => gameProcess("basicland"); return true
		  case "Land 2" => gameProcess("land2");return true
		  case "Land 3" => gameProcess("land3");return true
		  case "Land 4" => gameProcess("land4");return true
		  case "2" => helpView
		  case "3" => showMenu; return true
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen");
	   }
	   false
   }
   
   def gameProcess(mapName:String) 
   {
     mapChoice(mapName)
     showTUI
     var exit = false
     while(!exit)
     {
       exit = gameProcessInputLine(readLine())
     }
     if(exit)
     {
       println("Spiel wurde beendet")
     }
   }
   
   def gameProcessInputLine(input:String) =
   {
     var row = 0
     var column = 0
     var inputCharArray = input.toCharArray();
     var inputColumn = inputCharArray(0).toString
     var inputRow = inputCharArray(1).toString + inputCharArray(2).toString;
     inputColumn match
     {
       case "A" => column =0
       case "B" => column =1
       case "C" => column =2
       case "D" => column =3
       case "E" => column =4
       case "F" => column =5
       case "G" => column =6
       case "H" => column =7
       case "I" => column =8
       case "J" => column =9
       case "K" => column =10
       case "L" => column =11
       case "M" => column =12
       case "N" => column =13
       case "O" => column =14
       case "P" => column =15
       case "Q" => column =16
       case "R" => column =17
       case "2" => helpView
       case "3" =>  true
       case _ => println("Ihr Eingabe: " + input + "ist nicht korrekt, bitte Wiederholen")
     }
     inputRow match
     {
       case "01" => row =0
       case "02" => row =1
       case "03" => row =2
       case "04" => row =3
       case "05" => row =4
       case "06" => row =5
       case "07" => row =6
       case "08" => row =7
       case "09" => row =8
       case "10" => row =9
       case "2" => helpView
       case "3" =>  true
       case _ => println("Ihr Eingabe: " + input + "ist nicht korrekt, bitte Wiederholen")
     }
     println("Row: " + row + "Column: " +column)
     checkGameOver

   }
   
   def helpView() 
   {
     println("1 Map-Auswahl")
     println("Wahl einer Map mithilfe des namens. Zur Verfuegung stehen:")
     println("Basicland : Land 1 : Land 2 : Land 3")
     println("------------------------------------------------------------------------")
     println("Spiel: Feld Auswahl")
     println("Fuer die Auswahl eines Spielfeldes das Muster [Spalte][Zeile] verwenden.")
     println("Beispiel: Fuer das erste Feld (erste Spalte und erste Zeile) wähle:")
     println("A01")
     println("------------------------------------------------------------------------")
     println("2 Hilfe")
     println("Hilfsanzeige")
     println("------------------------------------------------------------------------")
     println("3 Exit")
     println("Das Spiel kann zu jedem Zeitpunkt mit der Taste 4 beendet werden.")
   }
   
   def checkGameOver() =
   {
     //TODO Prüfe ob Spiel vorüber
     false
   }
   
   def showAllMap() 
   {
     printM1;
     printM2;
     printM3;
     printM4;
   }
   
   def showMenu()
   {
     println("1 Map-Auswahl")
     println("2 Spiel starten")
     println("3 Hilfe")
     println("4 exit")
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
   
   def mapChoice(mapName:String) = 
   {
     game.mapPosition(mapName)
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