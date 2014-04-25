package main.scala.view
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import scala.io.Source._
import main.scala.util._
import main.scala.model.Avatar

class TUI (var game: Gamefield) extends Observable with Observer{
  
   val delimiterVertical = "|";
   val delimiterHorizontal: Char = '-';
   val labelHorizontal = Array[String] ("  A ", "  B ", "  C ", "  D ", "  E ", "  F ", "  G ", "  H ", "  I ", "  J ", "  K ", "  L ", "  M ", "  N ", "  O ", "  P ", "  Q ", "  R ")
   val labelVertical = Array[String] ("01", "02", "03", "04", "05", "06", "07", "08", "09","10")
  
   
   def showField = {
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
        	// ohne Farben
        	//print(delimiterVertical + game.world(j)(k-1).showImage + delimiterVertical)
        	
        	// mit Farben
        	if(game.world(j)(k-1).getHolder == -1)
        		print(delimiterVertical + Console.BLUE + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 0)
        		print(delimiterVertical + Console.RED + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 1)
        		print(delimiterVertical + Console.GREEN + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 2)
        		print(delimiterVertical + Console.YELLOW + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        
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
   
   def processInputLine(input:String):Boolean =
   {
     var isCorrect = false
	   input match
	   {
		  case "1" => isCorrect = mapProcess
		  case "2" => helpView;
		  case "Basicland" => sendMapChoice("basicland");isCorrect = true
		  case "basicland" => sendMapChoice("basicland");isCorrect = true
		  case "Land 2" => sendMapChoice("land2");isCorrect = true
		  case "land 2" => sendMapChoice("land2");isCorrect = true
		  case "Land 3" => sendMapChoice("land3");isCorrect = true
		  case "land 3" => sendMapChoice("land3");isCorrect = true
		  case "Land 4" => sendMapChoice("land4");isCorrect = true
		  case "land 4" => sendMapChoice("land4");isCorrect = true
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen")
	   }
     isCorrect
   }
   def mapProcess():Boolean =
   {
     
     var isInputCorrect=false
      while(!isInputCorrect)
     {
       showAllMap 
       isInputCorrect = mapProcessInputLine(readLine())
     }
     true
   }
   
   def mapProcessInputLine(input:String) : Boolean =
   {
	   input match
	   {
		  case "Basicland" => sendMapChoice("basicland"); return true
		  case "basicland" => sendMapChoice("basicland"); return true
		  case "Land 2" => sendMapChoice("land2");return true
		  case "land 2" => sendMapChoice("land2");return true
		  case "Land 3" => sendMapChoice("land3");return true
		  case "land 3" => sendMapChoice("land3");return true
		  case "Land 4" => sendMapChoice("land4");return true
		  case "land 4" => sendMapChoice("land4");return true
		  case "2" => helpView
//		  case "3" => showMenu; return true
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen");
	   }
	   false
   }
   
   def sendMapChoice(mapName:String) = 
   {
     var notify = new Notification(Notification.Map)
     notify.map = mapName
     notifyObservers(notify)
   }
   
   def gameProcess(mapName:String) 
   {
     sendMapChoice(mapName)
//     showField
     /* Diese Logik gehört mittels notification zum Controller
     var exit = false
     while(!exit)
     {
       exit = gameProcessInputLine(readLine())
     }
     if(exit)
     {
       gameOver
     }
     * 
     */
   }
   
   def gameOver()
   {
     println("Spiel wurde beendet")
     println("--------------------------------------------------------------------------------")
   }
   
   /**
    * Correct Length of the Input is either 1, 2 or 3 Sings.
    */
   def checkIsCorrectLength(input:String) : Boolean = 
   {
     if (input != null)
     {
       var inputCharArray = input.toCharArray();
       if(inputCharArray.length == 1 || inputCharArray.length == 3 || inputCharArray.length == 2)
       {
         return true
       }
     }
      false
   }
   
   def readResponse:Boolean = 
   {
     var loopBreak = false
     var response = false
     while(!loopBreak)
     {
       var input = readLine()
       loopBreak = true
       input match
       {
         case "ja" => response = true
         case "Ja" => response = true
         case "j" => response = true
         case "nein" => response = false
         case "Nein" => response = false
         case "n "=> response = false
         case _ => println("Keine korrekte Antwort. ja/nein ?"); loopBreak= false
       }
       
     }
     response
   }
   
   
   def readPosition : Position =
   {
     var loop = true
     var position:Position = null
     var isMessage = false
     var permissionMatch = true
     while(loop)
     {
         loop = false
         isMessage = false
         permissionMatch = true
    	 var input = readLine()
     
	     if(!checkIsCorrectLength(input))
	     {
	        isMessage = true
	        permissionMatch = false
	        loop = true
	     }
		 if(input.length == 1)
		 {
		   input match
		   {
		       case "2" => helpView; permissionMatch = false
		       case _ => isMessage = true; loop = true; permissionMatch = false
		//	       case "3" => return true
		 
		   }
		 }
		 if(permissionMatch)
		 {
		     var row = 0
		     var column = 0
		     var inputCharArray = input.toCharArray();
		     
		     var inputColumn = inputCharArray(0).toString
		     var inputRow:String = ""
		     if(inputCharArray.length > 2)
		     {
		    	 inputRow = inputCharArray(1).toString + inputCharArray(2).toString
		     }
		     else
		     {
		    	 inputRow = inputCharArray(1).toString
		     }
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
		       case "a" => column =0
		       case "b" => column =1
		       case "c" => column =2
		       case "d" => column =3
		       case "e" => column =4
		       case "f" => column =5
		       case "g" => column =6
		       case "h" => column =7
		       case "i" => column =8
		       case "j" => column =9
		       case "k" => column =10
		       case "l" => column =11
		       case "m" => column =12
		       case "n" => column =13
		       case "o" => column =14
		       case "p" => column =15
		       case "q" => column =16
		       case "r" => column =17
		       case _ => isMessage = true; loop =true
		     }
		     inputRow match
		     {
		       case "1" => row =0
		       case "2" => row =1
		       case "3" => row =2
		       case "4" => row =3
		       case "5" => row =4
		       case "6" => row =5
		       case "7" => row =6
		       case "8" => row =7
		       case "9" => row =8
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
		       case _ => isMessage = true; loop = true
		     }
		     position = new Position(row,column)
		 }
		 
		 if(isMessage)
		     {
		       println("Ihr Eingabe: " + input + " ist nicht korrekt, bitte Wiederholen")
		     }
		 
     }
     
      position
     	
   }
   
   /**
    * Read from user an amount so long as the amount is an Number.
    * When the Input was wrong the tui will give a message.
    * @return number of the army
    */ 
	 def deliverArmyCount():Int =
   {
     var ok = false
     var input = ""
     while(!ok)
     {
       input = readLine()
       ok = isNumber(input)
       if(!ok)
       {
         println("Eingabe ungueltig, eine Zahl wird benoetigt.")
         println("Bitte wiederhole die Eingabe korrekt.")
       }
     }
     input.toInt  
   }
   
   
   
   
   def isNumber(input: String) = input forall Character.isDigit
   
   
   def helpView() 
   {
     println("1 Map-Auswahl")
     println("Wahl einer Map mithilfe des namens. Zur Verfuegung stehen:")
     println("Basicland : Land 1 : Land 2 : Land 3")
     println("--------------------------------------------------------------------------------")
     println("Spiel: Feld Auswahl")
     println("Fuer die Auswahl eines Spielfeldes das Muster [Spalte][Zeile] verwenden.")
     println("Beispiel: Fuer das erste Feld (erste Spalte und erste Zeile) wähle:")
     println("A01")
     println("--------------------------------------------------------------------------------")
     println("2 Hilfe")
     println("Hilfsanzeige")
     println("--------------------------------------------------------------------------------")
     println("3 Exit")
     println("Das Spiel kann zu jedem Zeitpunkt mit der Eingabe 3 beendet werden.")
   }
   
   
   def showAllMap() 
   {
     printM1;
     printM2;
     printM3;
     printM4;
   }
   def startTUI()
   {
     showMenu
     while(!processInputLine(readLine())) {}  
   }
   
   def showMenu()
   {
     println("1 Map-Auswahl")
     println("2 Hilfe")
     println("3 exit")
     println("--------------------------------------------------------------------------------")
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
   
   override def updateObserver(notification:Notification)
   {
     notification.typ match
	   {
		  case Notification.Reinforcement => reinforcementProcess(notification)
		  case Notification.Battle => battleProcess(notification)
		  case Notification.Attack => attackProcess
		  case Notification.Message => infoProcess(notification)
		  case Notification.Question => questionProcess(notification)
		  case Notification.Tactic => tacticProcess(notification)
		  case Notification.Army => armyProcess(notification)
		  case _ => println("Debug: Falsche Notification")
	   }
   }
   
   def armyProcess(n:Notification)
   {
     n.value = deliverArmyCount
     notifyObservers(n)
   }
   
   def tacticProcess(n:Notification)
   {
     n.position = readPosition
     notifyObservers(n)
   }
   
   def questionProcess(n:Notification)
   {
     n.question = readResponse
     notifyObservers(n)
   }
   
   def infoProcess(n:Notification)
   {
     println(n.message)
   }
   
   
   def battleProcess(notification:Notification)
   {
     var notificationNew = new Notification(Notification.Battle)
     notificationNew.position = readPosition
     notificationNew.currentPlayer = notification.currentPlayer
     notificationNew.isOwnLand = notification.isOwnLand
     notifyObservers(notificationNew)
     
   }
   
   def attackProcess
   {
     var notification = new Notification(Notification.Attack)
     notification.value = deliverArmyCount
     notifyObservers(notification)
     
   }
   
   def reinforcementProcess(notification:Notification)
   {
	   sendReinforcementChoice(readPosition, notification.currentPlayer)
   }
   
   
   def sendReinforcementChoice(position:Position, player:Avatar)
   {
     var notification = new Notification(Notification.Reinforcement)
     notification.position = position
     notification.currentPlayer = player
     notifyObservers(notification)
   }
   
   def sendPositionChoice(position:Position)
   {
     var notification = new Notification(Notification.Position)
     notification.position = position
     notifyObservers(notification)
   }
}