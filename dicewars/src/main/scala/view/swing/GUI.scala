package main.scala.view.swing

// own packages
//import main.scala.controller.DicewarController
import main.scala.model._
//import main.scala.util.Message
//import main.scala.util.Notification
import main.scala.util._
import main.scala.view._

// standard packages
import scala.swing._
//import scala.swing.Swing.LineBorder
import scala.swing.event.WindowClosing
//import scala.swing.TextField
//import scala.swing.event.KeyPressed
//import scala.swing.event.Key
import javax.swing.JOptionPane


class GUI(val game:Gamefield) extends Frame with View {
	// frametitel
	title = "Dicewars"
	  
	val swingView = this
	// panel inits.
	var fieldPanel:FieldPanel =  null
	var menuPanel:MenuPanel = null
//	// selected fieldposition
//	var position :WorldPosition = null
//	// flag to beckon to send a notification to the observer
//	var mapChoosenFlag:Boolean = false
//	var fieldFlag:Boolean = false
//	var incomingInput:Boolean = false
	/*
	 * MapChoicePanel is a abstract class, so to define a normal object with new is forbidden
	 * Solution: case Classes used 
	 * */
	var mapPanel:MapChoicePanel = new MapChoicePanel("Mapauswahl")
	{
		def notification(mapName:String) = new MapSelectedEvent(mapName)
	}
	
	
	/*
	 * actions corresponding the actions form the panels 
	 * */
	reactions +=
	{
	  case MapSelectedEvent(mapName) => sendMapChoice(mapName);  //listenTo(fieldPanel);selectPanel(fieldPanel)
	  case MapChoice() => sendMapSelctionMenu //; listenTo(mapPanel); selectPanel(mapPanel)
	  case FieldSelectedEvent(position) => sendPosition(position)
	  case CloseEvent() => closeView
	  case WindowClosing(_) =>closeView
	}
	

	menuBar = new MenuBar
	{
		contents += new Menu("Game")
		{
			contents += new MenuItem(Action("Neues Spiel starten")
			{
			  //resetGame
			})
			contents += new MenuItem(Action("Runde beenden")
			{
				// sendRoundEnd
			})
			contents += new MenuItem(Action("Quit")
			{
			  closeView
			})
		}
		contents += new Menu("Actions"){}
	}
	
	
	def InputUnitAmount =
	{		
		val input= new DialogPanel().amount.getOrElse(throw new IllegalStateException("Wrong input!!"))
		var response = input.amount
		sendAmountOfUnit(response.toInt)
	}
	
	
	def readResponse:Boolean =
    {	
		var response = false
		var eingabe = JOptionPane.showConfirmDialog(null,
                                                            "Fortfahren?",
                                                            "",
                                                            JOptionPane.YES_NO_OPTION);
		println(eingabe)
		if(eingabe == 0)
		  response = true
		response
    }
	

	/*
	 * close this view
	 * 1. step: send message to observer to detach this object
	 * 2. step: close GUI
	 * */
	def closeView 
	{
	    // dieses View aus der Liste entfernen.
		// tui ebenfalls schließen (noti.)
	    dispose
	}
	
	override def startView()
	{
		if(fieldPanel == null)
    		fieldPanel = new FieldPanel(game)
		listenTo(fieldPanel)
		if(menuPanel == null)
			menuPanel = new MenuPanel("Spiel Start")
		listenTo(menuPanel)
		listenTo(mapPanel)
		showCoverMenu

//		if(!mapChoosenFlag)
//		{
//			
//
//		}
//		else
//			selectPanel(fieldPanel)
//		while(!mapChoosenFlag && !incomingInput){
//		  listenTo(menuPanel)
//		  sendMenu
//		}
//		incomingInput = false
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
   	 
   	 
   	def sendPosition(position:WorldPosition)
    {
      var n = new Notification(Notification.Position)
      n.position = position
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
	     case Notification.MapSample => displayMapSelection
	     case Notification.Input => 
	     case Notification.Move => 
	     case Notification.Question => questionResponse
	     case Notification.Message => messageProcess(notification)
	     case Notification.DrawUI => showField
	     case _ => println("Debug: Falsche Notification")
	   }
	}
	
	
	/*
	 * Display functions
	 * */
	
	/*
	 * shows choice of maps
	 * */
	def displayMapSelection
    {
		selectPanel(menuPanel)
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
	
	
	def messagePrintln(color:String, messageContent:String)
	{
		//println(color + messageContent + Console.RESET )
		//new DialogMessagePanel(messageContent)
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
	
	
	/*
	 * displays the gamefield
	 * */
	 def showField = 
      {
		selectPanel(fieldPanel)
      }
	
	 
	/*
	 * Help functions
	 * */
	
	 
	def questionResponse
	{
		sendAnswer(readResponse)
	}
	 
	
	/*
	 * display the selected Panel (Field/Menu/Dialog)
	 * standard Framesize with width = 280 and height = 340   
	 * */
	def selectPanel(panel:Panel)
	{
		visible = false
		minimumSize = new Dimension(340, 280)
		contents =  panel
		visible = true
	}
	
	
	def showCoverMenu
	{
		selectPanel(menuPanel)
	}
}

//	
//	def armyProcess(n:Notification)
//	{
//		n.value = deliverArmyCount
//		notifyObservers(n)
//		incomingInput = false
//	}
//   
//	def battleAssignProcess(notification:Notification)
//	{
//		var notificationNew = new Notification(Notification.BattleAssign) 
//		notificationNew.position = readPosition
//		notificationNew.currentPlayer = notification.currentPlayer
//		notificationNew.isFromLand = notification.isFromLand
//		notifyObservers(notificationNew)
//		incomingInput = false
//	}
//   
//	def battleAttackProcess
//	{
//		var notification = new Notification(Notification.BattleAttack)
//		notification.value = deliverArmyCount
//		notifyObservers(notification)
//		incomingInput = false
//    }
//	
//	
//
//	def mapSampleProcess
//	{
//		
//		incomingInput = false
//	}
//   

//	   
//	
//	def readPosition: WorldPosition =
//	{
//		var loop = true
//		//warten auf action !!! besser lösung finden?!!
//		while(!fieldFlag  && incomingInput)
//		{
//			listenTo(fieldPanel)
//		}
//		fieldFlag = false
//		incomingInput = false
//		position
//	}
//	
//   
//	def reinforcementProcess(notification:Notification)
//	{
//		sendReinforcementChoice(readPosition, notification.currentPlayer)
//		
//	}
//   
//	def resetGame
//	{
//		selectPanel(menuPanel)
//		position = null
//		mapChoosenFlag = false
//		fieldFlag = false
//		sendNewGame
//		incomingInput = false
//	}
//	

//	
//	/*
//	 * 
//	 * */
//	def sendMapChoice(mapName:String) = 
//	{
//		var notify = new Notification(Notification.Map)
//		notify.map = mapName
//		notifyObservers(notify)
//		incomingInput = false
//	}
//	def sendMapSample
//	{
//		var notify = new Notification(Notification.MapSample)
//		notifyObservers(notify)
//		incomingInput = false
//	}
//	
//	def sendMenu
//	{
//		var notify = new Notification(Notification.Menu)
//		notifyObservers(notify)
//		incomingInput = false
//	}
//
//	def sendNewGame
//	{
//		var notify = new Notification(Notification.NewGame)
//		notifyObservers(notify)
//		incomingInput = false
//	}
//	
//	def sendReinforcementChoice(position:WorldPosition, player:Avatar)
//	{
//		var notification = new Notification(Notification.Reinforcement)
//		notification.position = position
//		notification.currentPlayer = player
//		notifyObservers(notification)
//		incomingInput = false
//	} 
//	
//	def setPosition(pos :WorldPosition):WorldPosition = 
//	{
//		this.position = pos
//		position
//	}
//   
//	def startView()
//	{
//    	if(fieldPanel == null)
//    		fieldPanel = new FieldPanel(game)
//		listenTo(fieldPanel)
//		if(menuPanel == null)
//			menuPanel = new MenuPanel("Spiel Start")
//		if(!mapChoosenFlag)
//		{
//			selectPanel(menuPanel)
//
//		}
//		else
//			selectPanel(fieldPanel)
//		while(!mapChoosenFlag && !incomingInput){
//		  listenTo(menuPanel)
//		  sendMenu
//		}
//		incomingInput = false
//	}
//   
//	def tacticProcess(n:Notification)
//	{
//		n.position = readPosition
//		notifyObservers(n)
//		incomingInput = false
//	}
//	
//	/*
//	 * reactions corresponding to notifictions form the other pattern
//	 * */
//	def updateObserver(notification:Notification)
//	{
//	   notification.typ match
//	   {
//       	  case Notification.Menu => incomingInput = false; //sendMenu
////       	  case Notification.Help => helpProcess
//       	  case Notification.MapSample => incomingInput = true; mapSampleProcess
//       	  case Notification.NewGame => incomingInput = true; resetGame
//		  case Notification.Reinforcement => incomingInput = true; reinforcementProcess(notification)
//		  case Notification.BattleAssign => incomingInput = true; battleAssignProcess(notification)
//		  case Notification.BattleAttack => incomingInput = true; battleAttackProcess
//		  case Notification.Message => incomingInput = true; messageProcess(notification)
//		  case Notification.Question => incomingInput = true; questionProcess(notification)
//		  case Notification.TacticAssign => incomingInput = true; tacticProcess(notification)
//		  case Notification.TacticArmy => incomingInput = true; armyProcess(notification)
//		  case Notification.DrawUI => incomingInput = true; selectPanel(fieldPanel)
//		  case _ => println("Debug: Falsche Notification" + notification.typ)
//	   }
//	}