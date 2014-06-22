package main.scala.view.swing

// scala packages 
import scala.swing._
import scala.swing.event.WindowClosing
import javax.swing.JOptionPane

// own costum packages
import main.scala.model._
import main.scala.util._
import main.scala.view._



object messages extends TextArea(rows = 5, columns = 80)

/*
 * Graphical User Interface contains a menu bar and some panels
 * parent class View and Frame
 * @ game: is the model of the Gamefield
 * */
class GUI(val game:Gamefield) extends Frame with View {
	// frametitle
	title = "Dicewars"
	// panel inits.
	var fieldPanel:FieldPanel =  null

	
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
		case MapSelectedEvent(mapName) => sendMapChoice(mapName);
		case FieldSelectedEvent(position) => sendPosition(position)
		case CloseEvent() => sendCloseApp
		case WindowClosing(_) => sendCloseApp
	}
	
	
	/*
	 * menubar of this frame contains reset and quit (... coming other features)
	 * */
	menuBar = new MenuBar
	{
		contents += new Menu("Game")
		{
			contents += new MenuItem(Action("Neues Spiel starten")
			{
			  resetGame
			})
			contents += new MenuItem(Action("Quit")
			{
			  sendCloseApp
			})
		}
		contents += new Menu("Actions"){}
	}


	/*
	 * close this view
	 * */
	def closeView 
	{
	    dispose
	}
	
	
	/*
	 * request an input from the user which containts the amount of units the user likes to move
	 * and read the user input and notify the observers about it
	 * */
	def inputUnitAmount =
	{		
		val input= new DialogPanel().amount.getOrElse(throw new IllegalStateException("Wrong input!!"))
		var response = input.amount
		sendAmountOfUnit(response.toInt)
	}
		
	
	/*
	 * request an input from the user, input needed are amount of human and AI player
	 * */
	def playerInitMessage 
	{
		var dialog = new DialogMessagePanel
		var temp = dialog.playInit.getOrElse(throw new IllegalStateException("Wrong input!!"))
		var playerAmount = temp.playerAmount
		var botAmount = temp.botAmount
		sendPlayerInit(playerAmount.toInt, botAmount.toInt)
	}
	
	
	/*
	 * request user to confirm with yes or no
	 * and read the user input and notify the observers about it
	 * */
	def readResponse:Boolean =
    {	
		var response = false
		var eingabe = JOptionPane.showConfirmDialog(null,
                                                            "Fortfahren?",
                                                            "",
                                                            JOptionPane.YES_NO_OPTION);
		if(eingabe == 0)
		  response = true
		response
    }
	
	
	/*
	 * restart the application and begin with the initializen phase
	 * */
	def resetGame
	{
		sendReset	
	}

	
	/*
	 * start this view
	 * */
	override def startView
	{
		if(fieldPanel == null)
    		fieldPanel = new FieldPanel(game)
		listenTo(fieldPanel)
		listenTo(mapPanel)
		selectPanel(mapPanel)
	}

	
	/* 
   	 *	Communication functions of this class
   	 **/
   
	/*
	 * send the input with the amount of the unit which should be moved
	 * @ amount: containts the user input
	 * */
    def sendAmountOfUnit(amount:Int)
    {
    	var notification = new Notification(Notification.Move)
    	notification.amount = amount
    	notification.inputType = "amount"
    	notifyObservers(notification)
    }
   
	
    /*
     * send the answer from the user
     * @ answer: containts the user input
     * */
	def sendAnswer(answer:Boolean)
    {    
		var notification  = new Notification(Notification.Answer)
		notification.answer = answer
		notification.inputType = "question"
		notifyObservers(notification)
    }
	
	
	/*
     * send the request to close this application
     * */
	def sendCloseApp =
	{
		var notification = new Notification(Notification.Exit)
		notifyObservers(notification)
	}
	
	
	/*
     * send the request to show all map examples
     * */
	def sendMapSelctionMenu =
    {
		var notification = new Notification(Notification.MapSample)
		notifyObservers(notification)
    }
	
	
	/*
     * send the name of the choosen map which the user want to play
     * @ mapName: containts the user input
     * */
   	def sendMapChoice(mapName:String) = 
    {
		var notification = new Notification(Notification.Map)
		notification.map = mapName
		notification.inputType = "map"
		notifyObservers(notification)
    }
   	 
	/*
     * send the request to reset the game
     * */ 
   	def sendReset
   	{
   	  	var notification = new Notification(Notification.Reset)
   	  	notifyObservers(notification)   	  
   	}
   	 
   	
   	/*
     * send the amount of human player and AI's
     * @ playerCount: containts the user input human player
     * @ botCount: containts the user input AI player
     * */
   	def sendPlayerInit(playerCount:Int, botCount:Int)
    {
   		var notification = new Notification(Notification.PlayerInit)
      	notification.playerCount = playerCount
      	notification.botCount = botCount
      	notification.inputType = "playerInit"
      	notifyObservers(notification)
    }
   	 
   	   	
   	/*
     * send the position of the selected land
     * @ position: [column/row] containts the user input 
     * */ 
   	def sendPosition(position:WorldPosition)
    {
	    var notification = new Notification(Notification.Position)
	    notification.position = position
	    notification.inputType = "position"
	    notifyObservers(notification)
    }
	
   	
   	/*
	 * reactions corresponding to notifictions form the other pattern
	 * */
	override def updateObserver(notification:Notification)
	{
		notification.typ match
		{
		 	case Notification.DrawUI => showField
		 	case Notification.Exit => closeView
		 	case Notification.Input => 
		 	case Notification.MapSample => displayMapSelection
		 	case Notification.Message => messageProcess(notification)
		 	case Notification.Move => inputUnitAmount
		 	case Notification.PlayerInit => playerInitMessage
		 	case Notification.Question => questionResponse
		 	case Notification.Reset => resetGame	     
		 	case _ => println("Debug: Falsche Notification")
		}
	}
	
	
	/*
	 * Display functions
	 * */
	
	/*
	 * shows examples of maps
	 * */
	def displayMapSelection
    {
		selectPanel(mapPanel)
    }
	
	
	/*
	 * handels the color in which the message should be displayed
	 * @ color: possible colors for the players
	 * @ messageContent: content of this message
	 * */
	def messagePrint(color:Avatar.ColorTyp, messageContent:String)
	{
		color match 
		{
			case Avatar.Blue => fieldPanel.showMsg(messageContent,2)
			case Avatar.Mangenta => fieldPanel.showMsg(messageContent,4)
			case Avatar.Green => fieldPanel.showMsg(messageContent,3)
			case _ => println("Color Fehler")
		}
	}
	
	
	/*
	 * display the Message on this frame
	 * @ outType: type of this message
	 * @ messageContent: containts the string with the content
	 * */
	def messagePrintln(outType:Int, messageContent:String)
	{
		fieldPanel.showMsg(messageContent, outType)
	}
   
	
	/*
	 * message handler needed for displaying the game messages
	 * @ messageNotification: contains the message type and the content
	 * */
	def messageProcess(messageNotification:Notification)
	{
		var messageTyp:Message.MessageTyp = messageNotification.message.typ
		var messageContent:String = messageNotification.message.content
		messageTyp match
		{
			case Message.Success => messagePrintln(2, messageContent)
			case Message.Error => messagePrintln(1, messageContent)
			case Message.Info => messagePrintln(0, messageContent)
			case Message.Player => messagePrint(messageNotification.currentPlayer.color, messageContent)
			case _ => println("Debug: Falsche Notification")
		}
	}
	
	
	/*
	 * displays the gamefield
	 * */
	def showField =	selectPanel(fieldPanel)
	
	 
	/*
	 * Help functions
	 * */
	
	/*
	 * request to send the answer notification
	 * */ 
	def questionResponse = 	sendAnswer(readResponse)
	 
	
	/*
	 * display the selected Panel (Field/Menu/Dialog)
	 * standard Framesize with width = 280 and height = 340
	 * @ panel: contains the panel components which should be displayed   
	 * */
	def selectPanel(panel:Panel)
	{
		visible = false
		minimumSize = new Dimension(340, 280)
		contents =  panel
		visible = true
	}
}