package main.scala.controller

// own costum packages
import main.scala.model._
import main.scala.util._
import main.scala.view._
import main.scala.view.swing.GUI


/*
 * Class for the controller of this game.
 * This class is a subclass form Observer.
 * @ game: model of the gamefield
 * */
class DicewarController(val game:Gamefield) extends Observer {

  
	/*
	 * Remove this view as observer
	 * @ view: selected view
	 * */
	def detachView(view:View)
	{
		delegateExit
		game.removeObserver(view)
	}

  	/*
  	 * Starts the gamce
  	 * */
  	def startGame
  	{
  		game.gameHandler
  	}
	
  	
  	/*
  	 * End the game
  	 * */
	protected def exitGame
	{
		println("Applikation wurde beendet")
		System.exit(0)
	}
  
	
	/*
	 * Attach the view.
	 * @ view: selected view
	 * */
	def joinView(view:View)
	{
		game.addObserver(view)
		view.addObserver(this)
		view.startView
	}
  
	
	/*
	 * Reaction process for incoming notifications from the 
	 * object which is observed
	 * @ notification: Incoming notifiaction
	 * */
	override def updateObserver(notification:Notification)
	{
	   notification.typ match
	   {
	    	   case Notification.MapSample => delegateMapSelectionMenu
	    	   case Notification.Map =>  delegateMapSelection(notification)
	    	   case Notification.Position => delegatePosition(notification)
	    	   case Notification.Answer => delegateAnswer(notification)
	    	   case Notification.Move => delegateMove(notification)
	    	   case Notification.PlayerInit => delegatePlayerInit(notification)
	    	   case Notification.Exit => delegateExit
	    	   case Notification.Reset => delegateReset
	    	   case _ => println("Debug Controller: Falsche Notification")
	   }
	}

	
	/*
	 * Delegate the user response according the question
	 * @ notification: Incoming notification containing the need input and other information 
	 * */
	def delegateAnswer(notification:Notification)
    {
    	if(game.checkInputTypeValidation(notification.inputType))
    		game.questionResponse(notification.answer)
    	else
    	{
    		game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    		if(game.currentPhase !=3)
    		  game.currentPhase -= 1
    		game.gameHandler
    	}
    }
	
	
	/*
	 * Delegate the command to close this application
	 * */
	def delegateExit
	{
		game.sendExit
		exitGame
	}
	
	
	/*
	 * Delegate the selected map setup to the model
	 * @ notification: Incoming notification containing the need input and other information 
	 * */
	def delegateMapSelection(notification:Notification)
    {
		if(game.checkInputTypeValidation(notification.inputType))
    	{   		
    		       game.initGame(notification.map)
    		       game.currentPhase += 1
    	}
    	else
    	{
    	    game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    	}
		game.gameHandler
    }
	

	/*
	 * Delegate the command to show the map selection menu
	 * */
    def delegateMapSelectionMenu
    {
      game.startShowMapSelectionMenu
    }
    
    
    /*
	 * Delegate the amount of units choosen by the player.
	 * @ notification: Incoming notification containing the need input and other information 
	 * */
    def delegateMove(notification:Notification)
    {
    	if(game.checkInputTypeValidation(notification.inputType))
    		game.manageUnitMove(notification.amount)
    	else
    	{
    	    game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    	    if(game.currentPhase !=3)
    	    	game.currentPhase -= 1
    	    game.gameHandler
    	}
    }
    
    
    /*
	 * Delegate the selected territory information.
	 * @ notification: Incoming notification containing the need input and other information 
	 * */
    def delegatePosition(notification:Notification)
    {
      var pos = notification.position
      if(game.checkInputTypeValidation(notification.inputType) && game.checkPositionConstrains(pos))
        game.setLandSelection(pos)
      else
      {
        game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
        if(game.currentPhase !=3 && game.currentPhase != 8 && game.currentPhase != 20)
        	game.currentPhase -= 1
        game.gameHandler
      }
    }
    
    
    /*
	 * Delegate the player config information.
	 * @ notification: Incoming notification containing the need input and other information 
	 * */
    def delegatePlayerInit(notification:Notification)
    {
    	if(game.checkInputTypeValidation(notification.inputType))
    	{
    	  if(game.sendPlayerConfigMessage(notification.playerCount, notification.botCount))
    	  {
    	   game.initPlayer(notification.playerCount, notification.botCount)
    	   game.currentPlayer = game.avatarContainer(0)
    	   game.currentPhase += 1
    	  }
    	  game.gameHandler
    	} 
    	else
    	{
    		game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    		game.gameHandler
    	}
    }
    
    
    /*
     * Delegate the command to reset the game.
     * */
    def delegateReset
    {
      game.initPhase
    }  
}