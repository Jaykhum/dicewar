package main.scala.view
import main.scala.model.Avatar
import main.scala.model.Gamefield
import main.scala.model.World
import main.scala.util._

//import main.scala.controller.DicewarController
//import scala.io.Source._

import util.matching.Regex
import main.scala.model.WorldPosition

class TUI (var game: Gamefield) extends View
{
	val delimiterVertical = "|";
	val delimiterHorizontal: Char = '-';
	val labelHorizontal = Array[String] ("  A ", "  B ", "  C ", "  D ", "  E ", "  F ", "  G ", "  H ", "  I ", "  J ", "  K ", "  L ", "  M ", "  N ", "  O ", "  P ", "  Q ", "  R ")
	val labelVertical = Array[String] ("01", "02", "03", "04", "05", "06", "07", "08", "09","10")
	val inputThread = new Thread(new Runnable {
	    override def run() {
		      try {
		        while(readConsoleInput){}
		      } catch {
		        case ex: Exception => 
		      }
		    }
    })
	
	
	/*
	 * process functions
	 **/
	
	override def closeView
	{
	  
	}
	
	override def startView
	{
		helpView
		inputThread.start
	}
       	
	
	def readConsoleInput: Boolean = 
   	{
   	  	val playRegex = new Regex("^([a-rA-R]) (\\d)$", "col", "row")
   	  	val startRegex = new Regex("^start ([basicland|Basicland|land2|Land2|land 2|Land 2|land3|Land3|land 3|Land 3|land4|Land4|land 4|Land 4]+?)$", "map")
   	  	val playerRegex = new Regex("^spieler (\\d), bot (\\d)$", "playerCount", "botCount")
   	  	val answerRegex = new Regex("(j|J|n|N|ja|Ja|nein|Nein)", "response")
   	  	val amountRegex = new Regex("^(\\d+)$", "amount")
   	  	var continue = true
   	  	readLine match {
	   		case ("h"|"hilfe") => helpView; readConsoleInput
			case ("m"| "map") => sendMapSelctionMenu
			case ("q"| "quit") => stop ; continue = false;
   	  	  	case answerRegex(response) => questionResponse(response)
   	  	  	case playerRegex (playerCount, botCount) => sendPlayerInit(playerCount.toInt, botCount.toInt)
			case startRegex(mapName) => checkMapName(mapName)
			case playRegex(col, row) => sendPosition(replaceColData(col), row.toInt)
			case amountRegex(amount) => sendAmountOfUnit(amount.toInt)
			case _ => println("Fehlerhafte Eingabe. Bitte versuchen Sie es erneut!")
	   	}
   	  	continue 
   	}
	
   	/* 
   	 *	Communication functions 
   	 **/
   
   
    def sendAmountOfUnit(amount:Int)
    {
      var n = new Notification(Notification.Move)
      n.amount = amount
      n.inputType = "amount"
      notifyObservers(n)
    }
   
	
	def sendAnswer(answer:Boolean)
    {    
      var n = new Notification(Notification.Answer)
      n.answer = answer
      n.inputType = "question"
      notifyObservers(n)
    }
	
	
	def sendMapSelctionMenu =
    {
      var notification = new Notification(Notification.MapSample)
      notifyObservers(notification)
    }
	
	
   	 def sendMapChoice(mapName:String) = 
   	{
     var notify = new Notification(Notification.Map)
     notify.map = mapName
     notify.inputType = "map"
     notifyObservers(notify)
   }
   	 
   	 
   	def sendPlayerInit(playerCount:Int, botCount:Int)
    {
      var n = new Notification(Notification.PlayerInit)
      n.playerCount = playerCount
      n.botCount = botCount
      n.inputType = "playerInit"
      notifyObservers(n)
    }
   	
   	 
   	def sendPosition(col:Int, row:Int)
    {
      var n = new Notification(Notification.Position)
      n.position = new WorldPosition(row-1, col-1)
      n.inputType = "position"
      notifyObservers(n)
    }
   	

	
   	
   	/*
	 * reactions corresponding to notifictions form the other pattern
	 * */
	override def updateObserver(notification:Notification)
	{
	   notification.typ match
	   {
	     case Notification.MapSample =>  displayMapSelection
	     case Notification.Input => //readConsoleInput
	     case Notification.Move => 
	     case Notification.Question => 
	     case Notification.Message =>  messageProcess(notification)
	     case Notification.DrawUI =>  showField
	     case _ => println("Debug: Falsche Notification")
	   }
	}
	
	
	/*
	 * Display functions
	 * */
	
	def displayMapSelection
    {
   	  printM1; printM2; printM3; printM4;
//   	  helpView
    }
	
	
	def messageProcess(messageNotification:Notification)
   {
     var messageTyp:Message.MessageTyp = messageNotification.message.typ
     var messageContent:String = messageNotification.message.content
     messageTyp match
	   {
		  case Message.Success => messagePrintln(Console.GREEN, messageContent)
		  case Message.Error => messagePrintln(Console.RED, messageContent)
		  case Message.Info => messagePrintln(Console.WHITE, messageContent)
		  case Message.Player => messagePrint(messageNotification.currentPlayer.color, messageContent)
		  case _ => println("Debug: Falsche Notification")
	   }
     
   }
   
   def messagePrintln(color:String, messageContent:String)
   {
     println(color + messageContent + Console.RESET )
   }
   
   def messagePrint(color:Avatar.ColorTyp, messageContent:String)
   {
     color match 
     {
       case Avatar.Yellow => print(Console.YELLOW + messageContent + Console.RESET )
       case Avatar.Mangenta => print(Console.MAGENTA + messageContent + Console.RESET)
       case Avatar.Green => print(Console.GREEN + messageContent + Console.RESET)
       case _ => println("Color Fehler")
     } 
   }
	
	
	def printM1
	{
     println("Basicland")
     println("------------")
     println("|          |")  
     println("|   $$$$   |")
     println("|    $$$$  |")
     println("|          |")
     println("------------")
   }
   
   def printM2 
   {
     println("Land 2")
     println("------------")
     println("|       $$$|")  
     println("|     $$$$$|")
     println("|   $$$$$$$|")
     println("|$$$$$$$$$$|")
     println("------------")
   }
   
   def printM3
   {
     println("Land 3")
     println("------------")
     println("|  $$$$$$$$|")  
     println("| $$$   $$$|")
     println("|$$   $$$$ |")
     println("|$$$$$$$$  |")
     println("------------")
   }
   
   def printM4
   {
     println("Land 4")
     println("------------")
     println("|$$$$$$$$$$|")  
     println("|  $$  $$$$|")
     println("|  $$  $$$$|")
     println("|$$$$$$$$$$|")
     println("------------")
   }
   
      def showField = 
      {
     // label top
     print("    ")
     for(h <- 0 until World.width)
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
     
      for( j <- 0 until World.height; k <- 0  to World.width+1){

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
//        	print(delimiterVertical + game.world(j)(k-1).showImage + delimiterVertical)
          
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
     for(h <- 0 until World.width)
     {
       print(labelHorizontal(h))
     }
     println()
    }
   
      
    def stop = 
   	{
   	  println("Spiel Ende!")
   	  //controller.detachView(this)
   	}
    
    
    /*
     * help functions
     * */
   
   
    def checkMapName(mapName:String) =
   	{
   		mapName match
   		{
   			case ("basicland"|"Basicland"|"land2"|"Land2"|"land 2"|"Land 2"|"land3"|"Land3"|"land 3"|"Land 3"|"land4"|"Land4"|"land 4"|"Land 4") => sendMapChoice(mapName.replace(" ", ""))
   			case _ => println("Unbekannte Karte. Bitte Eingabe wiederholen!")
   		}
   	}
        
    
    def helpLimiter() 
    {
    	print("    ")
    	for(i <- 4 to 80-4)
    	{
    		print("-")
    	}
    	print("     \n")
   }
   	
	
	private def helpView() =
   	{
   	  	 println("h|hilfe: Hilfe")
	     println("Hilfsanzeige")
	     println("--------------------------------------------------------------------------------")
	     println("m|map: Map-Auswahl")
	     println("Zeige Karten die zur Verfuegung stehen:")
	     println("--------------------------------------------------------------------------------")
	     println("start <Kartenname>")
	     println("Startet ein neues Spiel mit gewählter Karte")
	     println("--------------------------------------------------------------------------------")
	     println("<Spalte> <Zeile>: Feld Auswahl")
	     println("Fuer die Auswahl eines Spielfeldes das Muster [Spalte][Zeile] verwenden.")
	     println("Beispiele: Fuer das erste Feld (erste Spalte und erste Zeile) wähle:")
	     println("A 01 " + "oder " + "A 1 " + "a 01 " + "oder " + "a 1")
	     println("--------------------------------------------------------------------------------")
	     println("<Zahl>: Anzahl Einheiten")
	     println("Anzahl der Einheiten die sie gerne verschieben moechten")
	     println("--------------------------------------------------------------------------------")
	     println("j|ja|Ja|n|nein|Nein: Antwortmoeglichkeiten")
	     println("Moegliche Antworten Sie eingeben koennen auf bestimmte Fragen")
	     println("--------------------------------------------------------------------------------")
	     println("q|quit: Exit")
	     println("Das Spiel kann zu jedem Zeitpunkt mit der Eingabe q beendet werden.")
   	}
    
    
           	 def questionResponse(response:String) =
       	{
	       	response match
	      	{ 
	      		case ("n"|"N"|"nein"|"Nein") => sendAnswer(false)
	      		case _ => sendAnswer(true)
	      	}
       	}
           		
	
	   def replaceColData(text:String):Int =  text match
   	   {case ("a"|"A") => 1  case ("b"|"B") => 2  case ("c"|"C") => 3  case ("d"|"D") => 4
   	    case ("e"|"E") => 5  case ("f"|"F") => 6  case ("g"|"G") => 7  case ("h"|"H") => 8
   	    case ("i"|"I") => 9  case ("j"|"J") => 10 case ("k"|"K") => 11 case ("l"|"L") => 12
   	    case ("m"|"M") => 13 case ("n"|"N") => 14 case ("o"|"O") => 15 case ("p"|"P") => 16
   	    case ("q"|"Q") => 17 case ("r"|"R") => 18 }
}