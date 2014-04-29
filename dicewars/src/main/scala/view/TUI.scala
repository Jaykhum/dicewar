package main.scala.view
import main.scala.controller.DicewarController
import main.scala.model.Gamefield
import scala.io.Source._
import main.scala.util._
import main.scala.model.Avatar
import main.scala.model.WorldPosition

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
          
          if(game.fromLand != null && game.fromLand == game.world(j)(k-1))
          {
            
            
        	if(game.world(j)(k-1).getHolder == -1)
        		print(delimiterVertical + Console.CYAN + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 0)
        		print(delimiterVertical + Console.YELLOW + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 1)
        		print(delimiterVertical + Console.MAGENTA + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 2)
        		print(delimiterVertical + Console.GREEN + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        		
        		
          }else if(game.toLand != null && game.toLand == game.world(j)(k-1))
          {
            
            
        	if(game.world(j)(k-1).getHolder == -1)
        		print(delimiterVertical + Console.CYAN + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 0)
        		print(delimiterVertical + Console.YELLOW + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 1)
        		print(delimiterVertical + Console.MAGENTA + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 2)
        		print(delimiterVertical + Console.GREEN + Console.WHITE_B + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        		
        		
          }else
          {
            
        	// mit Farben
        	if(game.world(j)(k-1).getHolder == -1)
        		print(delimiterVertical + Console.CYAN + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 0)
        		print(delimiterVertical + Console.YELLOW + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 1)
        		print(delimiterVertical + Console.MAGENTA + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        	else if(game.world(j)(k-1).getHolder == 2)
        		print(delimiterVertical + Console.GREEN + game.world(j)(k-1).showImage + Console.RESET + delimiterVertical)
        		
          }
        
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
   
   def menueProcessInputLine(input:String):Boolean =
   {
     var isCorrect = true
	   input match
	   {
		  case "1" => sendMapSample
		  case "2" => sendHelp; isCorrect = false
		  case "Basicland" => sendMapChoice("basicland")
		  case "basicland" => sendMapChoice("basicland")
		  case "Land 2" => sendMapChoice("land2")
		  case "land 2" => sendMapChoice("land2")
		  case "Land 3" => sendMapChoice("land3")
		  case "land 3" => sendMapChoice("land3")
		  case "Land 4" => sendMapChoice("land4")
		  case "land 4" => sendMapChoice("land4")
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen"); isCorrect = false
	   }
     isCorrect
   }
   
   def sendHelp
   {
     var notify = new Notification(Notification.Help)
     notifyObservers(notify)
   }
   
   def sendMapSample
   {
     var notify = new Notification(Notification.MapSample)
     notifyObservers(notify)
   }
   
   def sendMapChoice(mapName:String) = 
   {
     var notify = new Notification(Notification.Map)
     notify.map = mapName
     notifyObservers(notify)
   }
   

   def mapProcess():Boolean =
   {
     
     var isInputCorrect=false
      while(!isInputCorrect)
     {
       isInputCorrect = mapProcessInputLine(readLine())
     }
     true
   }
   
   def mapProcessInputLine(input:String) : Boolean =
   {
     var isCorrect = true
	   input match
	   {
		  case "Basicland" => sendMapChoice("basicland")
		  case "basicland" => sendMapChoice("basicland")
		  case "Land 2" => sendMapChoice("land2")
		  case "land 2" => sendMapChoice("land2")
		  case "Land 3" => sendMapChoice("land3")
		  case "land 3" => sendMapChoice("land3")
		  case "Land 4" => sendMapChoice("land4")
		  case "land 4" => sendMapChoice("land4")
		  case "2" => sendHelp; isCorrect = false
//		  case "3" => showMenu; return true
		  case _ => println("Falsche Eingabe, bitte korrekt Wiederholen");isCorrect = false
	   }
	   isCorrect
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
         case _ => println("Keine korrekte Antwort.\nWeiter Angreifen ja/nein ?"); loopBreak= false
       }
       
     }
     response
   }
   
   
   def readPosition : WorldPosition =
   {
     var loop = true
     var position:WorldPosition = null
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
		     position = new WorldPosition(row,column)
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
     println("Beispiele: Fuer das erste Feld (erste Spalte und erste Zeile) wähle:")
     println("A01 " + "oder " + "A1 " + "a01 " + "oder " + "a1")
     println("--------------------------------------------------------------------------------")
     println("2 Hilfe")
     println("Hilfsanzeige")
     println("--------------------------------------------------------------------------------")
     println("3 Exit")
     println("Das Spiel kann zu jedem Zeitpunkt mit der Eingabe 3 beendet werden.")
   }
   
   
   def showAllMapSample() 
   {
     printM1;
     printM2;
     printM3;
     printM4;
   }
   def menueProcess()
   {
     showMenu
     while(!menueProcessInputLine(readLine())) {}

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
       	  case Notification.Menu => menueProcess
       	  case Notification.Help => helpProcess
       	  case Notification.MapSample => mapSampleProcess
		  case Notification.Reinforcement => reinforcementProcess(notification)
		  case Notification.BattleAssign => battleAssignProcess(notification)
		  case Notification.BattleAttack => battleAttackProcess
		  case Notification.Message => messageProcess(notification)
		  case Notification.Question => questionProcess(notification)
		  case Notification.TacticAssign => tacticProcess(notification)
		  case Notification.TacticArmy => armyProcess(notification)
		  case Notification.UI => showField
		  case _ => println("Debug: Falsche Notification")
	   }
   }
   
   
   def mapSampleProcess
   {
     showAllMapSample
     mapProcess
   }
   
   def helpProcess
   {
     helpView
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
   
   def messageProcess(messageNotification:Notification)
   {
     var messageTyp:Message.MessageTyp = messageNotification.message.typ
     var messageContent:String = messageNotification.message.content
     messageTyp match
	   {
		  case Message.Success => messageprint(Console.GREEN, messageContent)
		  case Message.Error => messageprint(Console.RED, messageContent)
		  case Message.Info => messageprint(Console.WHITE, messageContent)
		  case _ => println("Debug: Falsche Notification")
	   }
     
   }
   
   def messageprint(color:String, messageContent:String)
   {
     println(color + messageContent + Console.RESET )
   }
   
   
   def battleAssignProcess(notification:Notification)
   {
     var notificationNew = new Notification(Notification.BattleAssign) 
  
     notificationNew.position = readPosition
     notificationNew.currentPlayer = notification.currentPlayer
     notificationNew.isFromLand = notification.isFromLand
     notifyObservers(notificationNew)
     
   }
   
   def battleAttackProcess
   {
     var notification = new Notification(Notification.BattleAttack)
     notification.value = deliverArmyCount
     notifyObservers(notification)
     
   }
   
   def reinforcementProcess(notification:Notification)
   {
	   sendReinforcementChoice(readPosition, notification.currentPlayer)
   }
   
   
   def sendReinforcementChoice(position:WorldPosition, player:Avatar)
   {
     var notification = new Notification(Notification.Reinforcement)
     notification.position = position
     notification.currentPlayer = player
     notifyObservers(notification)
   }
   

}