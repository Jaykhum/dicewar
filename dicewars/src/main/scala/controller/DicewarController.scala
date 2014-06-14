package main.scala.controller

import main.scala.model._
import main.scala.util._
import main.scala.view._
import main.scala.view.swing.GUI

class DicewarController(val game:Gamefield) extends Observer {
  
	game.gameHandler

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
	    	   case Notification.MapSample => delegateMapSelctionMenu
	    	   case Notification.Map =>  delegateMapSelction(notification)
	    	   case Notification.Position => delegatePosition(notification)
	    	   case Notification.Answer => delegateAnswer(notification)
	    	   case Notification.Move => delegateMove(notification)
	    	   case Notification.PlayerInit => delegatePlayerInit(notification)
	    	   
//       	  
//	   	  case Notification.Menu => delegateMenu 
//       	  
//       	  case Notification.NewGame => delegateNewGame
//       	  case Notification.Help => delegateHelp
//       	  case Notification.Exit => delegateExit
//		  
//		  
//		  
//		  case Notification.Reinforcement=> delegateReinforcement(notification)
//		  case Notification.BattleAssign=> delegateBattleAssign(notification)
//		  case Notification.BattleAttack=> delegateBattleAttack(notification)
//		  case Notification.Question=> delegateQuestion(notification)
//		  case Notification.TacticAssign=> delegateTacticAssign(notification)
//		  case Notification.TacticArmy=> delegateTacticArmy(notification)
		  case _ => println("Falsche Notification")
	   }
	}

	
	def delegateAnswer(notification:Notification)
    {
    	if(game.checkInputTypeValidation(notification.inputType))
    		game.questionResponse(notification.answer)
    	else
    	{
    		game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    		if(game.currentPhase !=2)
    		  game.currentPhase -= 1
    		game.gameHandler
    	}
    }
	
	
	def delegateMapSelction(notification:Notification)
    {
       //game.currentPlayer.inputType = notification.inputType
       game.initGame(notification.map)
    }
	

    def delegateMapSelctionMenu
    {
      game.startShowMapSelctionMenu
    }
    
    
    def delegateMove(notification:Notification)
   {
    	if(game.checkInputTypeValidation(notification.inputType))
    	{
    		
    		game.manageUnitMove(notification.amount)
    	}
    	else
    	{
    	    game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
    	    if(game.currentPhase !=2)
    	    	game.currentPhase -= 1
    	    game.gameHandler
    	}
   }
    
    
    def delegatePosition(notification:Notification)
    {
      var pos = notification.position
      //game.currentPlayer.inputType = notification.inputType
      if(game.checkInputTypeValidation(notification.inputType) && game.checkPositionConstrains(pos))
        game.setLandSelection(pos)
      else
      {
        game.sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
        if(game.currentPhase !=2 && game.currentPhase != 7 && game.currentPhase != 19)
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
    
    
}
//
//
//
//
//class DicewarController(val game:Gamefield) extends Observer {
//   
////    game.initWorld
////
////    tui.addObserver(this)
//////    gui.addObserver(this)
////    game.addObserver(tui)
////    game.addObserver(gui)
// 
////    var gui_thread = new Thread(new Runnable {
////		  def run() {
////		    gui.startView
////		  }
////    })
////    gui_thread.run()
////    gui.startView
//    
//    //game.startShowGameMenu
//    
//    //startGamePhase


//    

//    

//    
//    def delegateMenu
//    {
//      game.startShowGameMenu
//    }
//    

//    
//    def delegateHelp
//    {
//      game.startShowHelp
//    }
//    
//    def delegateExit
//    {
//     game.exitGame
//    }
//    
//     def delegateTacticArmy(n:Notification)
//    {
//        var from = game.fromLand
////        from.permissionMoveArmy = game.checkNumberOfUnitMove(from, n.value, n.minMove)
//        if(from.permissionMoveArmy)
//        	n.currentPlayer.newUnitsTemporary = n.value
//    }
//    
//    def delegateTacticAssign(notification:Notification)
//    {
//        notification.currentPlayer.inputCorrect = game.checkTacticLandSelection(notification.currentPlayer, notification.position, notification.isFromLand)
//        game.sendTacticAssignMessage(notification.currentPlayer, notification.position, notification.isFromLand)
//        var inputCorrect = notification.currentPlayer.inputCorrect
//        if(inputCorrect)
//        {
//          game.setFromOrTo(notification.currentPlayer, notification.position, notification.isFromLand)
//          game.sendNotificationUI
//        }
//    }
//    
////    def delegateQuestion(notification:Notification)
////    {
////     //(avatarContainer )Moegliche Fehlerquelle durch id auf zugriff array index, funkt nur weil index und id synchron sind=> Hash-Map besser
////     var player = game.avatarContainer(notification.currentPlayer.id) 
////     player.myTurn = notification.question
////    }
//    

//    
////    def delegateBattleAttack(notification:Notification)
////    {
////      game.fromLand.permissionMoveArmy = game.checkNumberOfUnitMove(game.fromLand, notification.value,notification.minMove)
////      
////      if(game.fromLand.permissionMoveArmy)         
////      game.setArmyForAttackAndDefenseLand(game.fromLand, game.toLand, notification.value)
////    }
////    
////    def delegateBattleAssign(notification:Notification)
////    { 
////    	notification.currentPlayer.inputCorrect = game.checkBattleLandSelection(notification.currentPlayer, notification.position, notification.isFromLand)
////        game.sendBattleAssignMessage(notification.currentPlayer, notification.position, notification.isFromLand)
////        
////        var inputCorrect = notification.currentPlayer.inputCorrect
////        if(inputCorrect)
////        {
////          game.setAttackOrDefenseLand(notification.position, notification.isFromLand)
////          game.sendNotificationUI
////        }
////    }
//    
//    
//    

//    
////    /**
////     * Start the Reinforcement-, Battle- and Tactical-phase foreach player.
////     */
////    def startGamePhase
////    {
////      while(!game.gameOver)
////      {
////	      for(i <- 0 to game.avatarContainer.size -1)
////	      {
////	          if(!game.avatarContainer(i).lost && !game.avatarContainer(i).isInstanceOf[Bot] )
////	          {
////	             game.startReinforcement(game.avatarContainer(i))
////	             game.startBattlePhase(game.avatarContainer(i))
////	             if(!game.gameOver)
////	             game.startTacticPhase(game.avatarContainer(i))
////	          }else if(!game.avatarContainer(i).lost && game.avatarContainer(i).isInstanceOf[Bot] )
////	          {
////	        	 var bot:Bot =  game.avatarContainer(i).asInstanceOf[Bot]
////	        	 bot.startReinforcementPhase(game.fieldContainer, game.world)
////	        	 bot.startBattlePhase(game.fieldContainer, game.world)
////	        	 if(!game.gameOver)
////	        	 bot.startTacticPhase(game.fieldContainer, game.world)
////	          }
////	      }
////      }
////      
////      var winnerPlayer = game.avatarContainer.find(p => !p.lost).get
////      game.sendNotificationGameOver(winnerPlayer)
////    }
//    
//    
//    def delegateReinforcement(notification:Notification)
//    {
//      game.handleReinforcement
//    }
//
//    def delegateNewGame
//    {
//    	game.newGame
//    	game.initWorld
//    	game.startShowGameMenu
//    }
//   }