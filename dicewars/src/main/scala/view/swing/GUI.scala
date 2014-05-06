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


class GUI(val game:Gamefield) extends Frame with View {

	title = "Dicewars"
	var fieldPanel:FieldPanel =  null
	var menuPanel:MenuPanel = null
	var mapPanel:MapChoicePanel = new MapChoicePanel("Mapauswahl")
	{
		def notification(mapName:String) = new MapSelectedEvent(mapName)
	}
	reactions +=
	{
	  case MapSelectedEvent(mapName) => sendMapChoice(mapName); selectPanel(fieldPanel)
	  case MapChoice() => sendMapSample; listenTo(mapPanel); selectPanel(mapPanel)
	  case CloseEvent() => closeView
	  case WindowClosing(_) =>closeView
	}
	
	def updateObserver(notification:Notification)
	{
//	       notification.typ match
//	   {
//       	  case Notification.Menu => menueProcess
//       	  case Notification.Help => helpProcess
//       	  case Notification.MapSample => mapSampleProcess
//		  case Notification.Reinforcement => reinforcementProcess(notification)
//		  case Notification.BattleAssign => battleAssignProcess(notification)
//		  case Notification.BattleAttack => battleAttackProcess
//		  case Notification.Message => messageProcess(notification)
//		  case Notification.Question => questionProcess(notification)
//		  case Notification.TacticAssign => tacticProcess(notification)
//		  case Notification.TacticArmy => armyProcess(notification)
//		  case Notification.DrawUI => showField
//		  case _ => println("Debug: Falsche Notification" + notification.typ)
//	   }
	}
	
	def deliverArmyCount:Int =
	{		
			val input= new DialogPanel().amount.getOrElse(throw new IllegalStateException("You should login!!!"))
			var response = input.amount
			response.toInt
	}
	
	def readResponse:Boolean =
    {
		
		var response = false


		//val auth = new DialogPanel().auth.getOrElse(throw new IllegalStateException("You should login!!!"))
		//println(auth)
		response
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
			  deliverArmyCount
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
   //  n.position = readPosition
     notifyObservers(n)
   }
   
   def questionProcess(n:Notification)
   {
     n.question = readResponse
     notifyObservers(n)
   }
   
}