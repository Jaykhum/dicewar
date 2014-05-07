package main.scala.view.swing

import main.scala.model.Gamefield
import main.scala.controller.DicewarController
import scala.swing._
import scala.swing.Swing.LineBorder
import scala.swing.event.WindowClosing
import main.scala.view._
import main.scala.util.Notification
import scala.swing.TextField
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import javax.swing.JOptionPane
import main.scala.model.WorldPosition
import main.scala.model.Avatar
import main.scala.util.Message


class GUI(val game:Gamefield) extends Frame with View {

	title = "Dicewars"
	var fieldPanel:FieldPanel =  null
	var menuPanel:MenuPanel = null
	var position :WorldPosition = null
	var mapPanel:MapChoicePanel = new MapChoicePanel("Mapauswahl")
	{
		def notification(mapName:String) = new MapSelectedEvent(mapName)
	}
	reactions +=
	{
	  case MapSelectedEvent(mapName) => sendMapChoice(mapName); selectPanel(fieldPanel)
	  case MapChoice() => sendMapSample; listenTo(mapPanel); selectPanel(mapPanel)
	  case FieldSelectedEvent(position) => setPosition(position); println("in GUI: "+ position.column + ","+position.row)
	  case CloseEvent() => closeView
	  case WindowClosing(_) =>closeView
	}
	
	def updateObserver(notification:Notification)
	{
	       notification.typ match
	   {
       	  case Notification.Menu => println("in GUI: menu")
//       	  case Notification.Help => helpProcess
       	  case Notification.MapSample => mapSampleProcess; println("in GUI:mapwahl")
		  case Notification.Reinforcement => reinforcementProcess(notification); println("in GUI: reinphase")
		  case Notification.BattleAssign => battleAssignProcess(notification); println("in GUI:battle")
		  case Notification.BattleAttack => battleAttackProcess;println("in GUI:attack")
		  case Notification.Message => messageProcess(notification); println("in GUI: message")
		  case Notification.Question => questionProcess(notification); println("in GUI:question")
		  case Notification.TacticAssign => tacticProcess(notification); println("in GUI:tatik")
		  case Notification.TacticArmy => armyProcess(notification); println("in GUI:army")
		  case Notification.DrawUI => selectPanel(fieldPanel); println("in GUI:draw")
		  case _ => println("Debug: Falsche Notification" + notification.typ); println("in GUI: wrong not")
	   }
	}
	
	def deliverArmyCount:Int =
	{		
			val input= new DialogPanel().amount.getOrElse(throw new IllegalStateException("Wrong input!!"))
			var response = input.amount
			println(response)
			response.toInt
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
 
   def setPosition(position :WorldPosition):WorldPosition = this.position
	
   def readPosition: WorldPosition =
   {
     println("in readPos: bla")
     var loop = true
     listenTo(fieldPanel)
     //warten auf action !!! besser lösung finden?!!
     while(loop){
       if(position != null)
         loop = false
     }
     println("in readPos: ende")
     position
   }
	
	def startView()
	{
		fieldPanel = new FieldPanel(game)
		listenTo(fieldPanel)
		menuPanel = new MenuPanel("Spiel Start")
		listenTo(menuPanel)
		if(!game.mapSelected)
			selectPanel(menuPanel)
		else
			selectPanel(fieldPanel)
		var loopFlag:Boolean =  true
//		while(loopFlag)
//		{
//			if(game.mapSelected)
//			  loopFlag =  false
//		}
	}

	val swingView = this
	menuBar = new MenuBar
	{
		contents += new Menu("Game")
		{
			contents += new MenuItem(Action("Neues Spiel starten")
			{
			  readResponse
			})
			contents += new MenuItem(Action("Runde beenden")
			{
			  new DialogMessagePanel("Error bla")
			})
			contents += new MenuItem(Action("Quit")
			{
			  closeView
			})
		}
		contents += new Menu("Actions"){}
	}

	def closeView 
	{
	    // dieses View aus der Liste entfernen.
		// tui ebenfalls schließen (noti.)
	    dispose
	}
	
	def selectPanel(panel:Panel)
	{
		visible = false
		minimumSize = new Dimension(340, 280)
		contents =  panel
		visible = true
	}
	/*
	val name:String = "Dicewars"
	val cells = Array.ofDim[CellPanel](game.height, game.width)
	
	def gridPanel = new GridPanel(game.height, game.width){
	  border = LineBorder(java.awt.Color.BLACK,2)
	  background = java.awt.Color.BLACK
	  for (i <- 0 to game.height; j <- 9 to game.width)
	  { 
		  contents += new Button(i.toString + " "+ j.toString)
	  }
	}
	
	//val statusline = new TextField(controller.statusText, 20)

	contents = new BorderPanel {
    //add(highlightpanel, BorderPanel.Position.North)
    add(gridPanel, BorderPanel.Position.Center)
    //add(statusline, BorderPanel.Position.South)
	}
	
	visible = true
	*/
   def mapSampleProcess
   {
     //println("in GUI:mapwahl")
     selectPanel(menuPanel)

   }
	
   def sendMapChoice(mapName:String) = 
   {
     var notify = new Notification(Notification.Map)
     println(mapName)
     notify.map = mapName
     notifyObservers(notify)
     //game.initGame(mapName)
   }
	
   def sendMapSample
   {
     var notify = new Notification(Notification.MapSample)
     notifyObservers(notify)
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
     this.position = null 
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
     new DialogMessagePanel(messageContent)
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