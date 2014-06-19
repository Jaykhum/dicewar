package main.scala.controller

import main.scala.model._
import main.scala.util._
import main.scala.view._
import main.scala.view.swing.GUI

class DicewarController(val game:Gamefield) extends Observer {

	def detachView(view:View)
	{
		delegateExit
		game.removeObserver(view)
	}

  	
  	def startGame
  	{
  		game.gameHandler
  	}
	    
	protected def exitGame
	{
		println("Applikation wurde beendet")
		System.exit(0)
	}
  
	def joinView(view:View)
	{
		game.addObserver(view)
		view.addObserver(this)
		view.startView
	}
  
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
	
	def delegateExit
	{
		game.sendExit
		exitGame
	}
	
	
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
	

    def delegateMapSelectionMenu
    {
      game.startShowMapSelectionMenu
    }
    
    
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
    
    def delegateReset
    {
      game.initPhase
    }  
}