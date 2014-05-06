package main.scala.controller
import main.scala.model._
import main.scala.view._
import main.scala.util._
import main.scala.view.swing.GUI

class DicewarController(val game:Gamefield, val tui:TUI, val gui:GUI) extends Observer {
   
    game.initWorld

    //tui.addObserver(this)
    gui.addObserver(this)
    //game.addObserver(tui)
    game.addObserver(gui)
    
    //gui.startView
    game.startShowGameMenu
    
    
    startGamePhase;
        
    override def updateObserver(notification:Notification)
    {
     notification.typ match
	   {
       	  case Notification.Menu => delegateMenu
       	  case Notification.MapSample =>delegateMapExample
       	  case Notification.Help => delegateHelp
       	  case Notification.Exit => delegateExit
		  case Notification.Map => initGame(notification)
		  case Notification.Reinforcement=>delegateReinforcement(notification)
		  case Notification.BattleAssign=>delegateBattleAssign(notification)
		  case Notification.BattleAttack=>delegateBattleAttack(notification)
		  case Notification.Question=>delegateQuestion(notification)
		  case Notification.TacticAssign=>delegateTacticAssign(notification)
		  case Notification.TacticArmy=>delegateTacticArmy(notification)
		  case _ => println("Falsche Notification")
	   }
    }
    
    
    def delegateMenu
    {
      game.startShowGameMenu
    }
    
    def delegateMapExample
    {
      game.startShowMapExample
    }
    
    def delegateHelp
    {
      game.startShowHelp
    }
    
    def delegateExit
    {
      //TODO exit function
    }
    
     def delegateTacticArmy(n:Notification)
    {
        var from = game.fromLand
        from.permissionMoveArmy = game.checkNumberOfUnitMove(from, n.value)
        if(from.permissionMoveArmy)
        n.currentPlayer.newUnitsTemporary = n.value
    }
    
    def delegateTacticAssign(notification:Notification)
    {
      println("test1")
        notification.currentPlayer.inputCorrect = game.checkTacticLandSelection(notification.currentPlayer, notification.position, notification.isFromLand)
        game.sendTacticAssignMessage(notification.currentPlayer, notification.position, notification.isFromLand)
        var inputCorrect = notification.currentPlayer.inputCorrect
        println("test2 inputCorrect" + inputCorrect)
        if(inputCorrect)
        {
          println("test3 notification.isFromLand" + notification.isFromLand)
          game.setFromOrTo(notification.currentPlayer, notification.position, notification.isFromLand)
          game.sendNotificationUI
        }
      
      
      
     
    }
    
    def delegateQuestion(notification:Notification)
    {
     //(avatarContainer )Moegliche Fehlerquelle durch id auf zugriff array index, funkt nur weil index und id synchron sind
     var player = game.avatarContainer(notification.currentPlayer.id) 
     player.myTurn = notification.question
    }
    
    def delegateBattleAttack(notification:Notification)
    {
      game.fromLand.permissionMoveArmy = game.checkNumberOfUnitMove(game.fromLand, notification.value)
      
      if(game.fromLand.permissionMoveArmy)
      game.setArmyForAttackAndDefenseLand(notification.value)
         
    }
    
    def delegateBattleAssign(notification:Notification)
    { 
    	notification.currentPlayer.inputCorrect = game.checkBattleLandSelection(notification.currentPlayer, notification.position, notification.isFromLand)
        game.sendBattleAssignMessage(notification.currentPlayer, notification.position, notification.isFromLand)
        
        var inputCorrect = notification.currentPlayer.inputCorrect
        if(inputCorrect)
        {
          game.setAttackOrDefenseLand(notification.position, notification.isFromLand)
          game.sendNotificationUI
        }
    
    }
    
    
    
    def initGame(notification:Notification)
    {
       game.initWorld
       game.initGame(notification.map)
    }
    
    /**
     * Start the Reinforcement-, Battle- and Tactical-phase foreach player.
     */
    def startGamePhase
    {
      while(!game.gameOver)
      {
	      for(i <- 0 to game.avatarContainer.size -1)
	      {
	          if(!game.avatarContainer(i).lost)
	          {
	             game.startReinforcement(game.avatarContainer(i))
	             game.startBattlePhase(game.avatarContainer(i))
	             if(!game.gameOver)
	             game.startTacticPhase(game.avatarContainer(i))
	          }
	      }
      }
      
      var winnerPlayer = game.avatarContainer.find(p => !p.lost).get
      game.sendNotificationGameOver(winnerPlayer)
      
    }
    
    
    def delegateReinforcement(notification:Notification)
    {
      game.handleReinforcement(notification.currentPlayer, notification.position)
    }

}