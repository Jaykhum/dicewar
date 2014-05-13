package main.scala.controller
import main.scala.model._
import main.scala.view._
import main.scala.util._
import main.scala.view.swing.GUI

class DicewarController(val game:Gamefield, val tui:TUI, val gui:GUI) extends Observer {
   
	var newGame: Boolean = false
    game.initWorld
    gui.addObserver(this)
    tui.addObserver(this)
    game.addObserver(gui)
    game.addObserver(tui)

 
    var gui_thread = new Thread(new Runnable {
		  def run() {
		    gui.startView
		  }
    })
    gui_thread.run()
    //gui.startView
    
    game.startShowGameMenu


    override def updateObserver(notification:Notification)
    {
     notification.typ match
	   {
       	  case Notification.Menu => delegateMenu 
       	  case Notification.MapSample => delegateMapExample
       	  case Notification.NewGame => delegateNewGame
       	  case Notification.Help => delegateHelp
       	  case Notification.Exit => delegateExit
		  case Notification.Map =>  initGame(notification)
		  case Notification.Reinforcement=> delegateReinforcement(notification)
		  case Notification.BattleAssign=> delegateBattleAssign(notification)
		  case Notification.BattleAttack=> delegateBattleAttack(notification)
		  case Notification.Question=> delegateQuestion(notification)
		  case Notification.TacticAssign=> delegateTacticAssign(notification)
		  case Notification.TacticArmy=> delegateTacticArmy(notification)
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
     game.exitGame
    }
    
     def delegateTacticArmy(n:Notification)
    {
        var from = game.fromLand
        from.permissionMoveArmy = game.checkNumberOfUnitMove(from, n.value, n.minMove)
        if(from.permissionMoveArmy)
        {
        	n.currentPlayer.newUnitsTemporary = n.value
        	game.inputFlag = "true"
        }
    }
    
    def delegateTacticAssign(notification:Notification)
    {
        notification.currentPlayer.inputCorrect = game.checkTacticLandSelection(notification.currentPlayer, notification.position, notification.isFromLand)
        game.sendTacticAssignMessage(notification.currentPlayer, notification.position, notification.isFromLand)
        var inputCorrect = notification.currentPlayer.inputCorrect
        if(inputCorrect)
        {
          game.setFromOrTo(notification.currentPlayer, notification.position, notification.isFromLand)
          game.sendNotificationUI
          game.inputFlag = "true"
        }
    }
    
    def delegateQuestion(notification:Notification)
    {
     //(avatarContainer )Moegliche Fehlerquelle durch id auf zugriff array index, funkt nur weil index und id synchron sind=> Hash-Map besser
     var player = game.avatarContainer(notification.currentPlayer.id) 
     player.myTurn = notification.question
     game.inputFlag = "true"
    }
    
    def delegateBattleAttack(notification:Notification)
    {
      game.fromLand.permissionMoveArmy = game.checkNumberOfUnitMove(game.fromLand, notification.value,notification.minMove)
      
      if(game.fromLand.permissionMoveArmy)
      {  
    	  game.setArmyForAttackAndDefenseLand(game.fromLand, game.toLand, notification.value)
    	  game.inputFlag = "true"
      }
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
          game.inputFlag = "true"
        }
    }
    
    
    
    def initGame(notification:Notification)
    {
       game.initWorld
       game.initGame(notification.map)

       startGamePhase
    }
    
    /**
     * Start the Reinforcement-, Battle- and Tactical-phase foreach player.
     */
    def startGamePhase
    {
      while(!game.gameOver && !newGame)
      {
	      for(i <- 0 to game.avatarContainer.size -1)
	      {
	          if(!game.avatarContainer(i).lost && !game.avatarContainer(i).isInstanceOf[Bot] )
	          {
	        	 game.inputFlag = "false"
	             game.startReinforcement(game.avatarContainer(i))
	             game.inputFlag = "false"
	             game.startBattlePhase(game.avatarContainer(i))
	             game.inputFlag = "false"
	             if(!game.gameOver)
	             game.startTacticPhase(game.avatarContainer(i))
	          }else if(!game.avatarContainer(i).lost && game.avatarContainer(i).isInstanceOf[Bot] )
	          {
	        	 var bot:Bot =  game.avatarContainer(i).asInstanceOf[Bot]
	        	 game.inputFlag = "false"
	        	 bot.startReinforcementPhase(game.fieldContainer, game.world)
	        	 game.inputFlag = "false"
	        	 bot.startBattlePhase(game.fieldContainer, game.world)
	        	 game.inputFlag = "false"
	        	 if(!game.gameOver)
	        	 bot.startTacticPhase(game.fieldContainer, game.world)
	          }
	      }
      }
      if (!newGame)
      {
	      var winnerPlayer = game.avatarContainer.find(p => !p.lost).get
	      game.sendNotificationGameOver(winnerPlayer)
      }
    }
    
    
    def delegateReinforcement(notification:Notification)
    {
      game.handleReinforcement(notification.currentPlayer, notification.position)
    }

    def delegateNewGame
    {
    	newGame = true
    	game.newGame
    	game.initWorld
    	game.newGameFlag = false
    	game.startShowGameMenu
    	startGamePhase

    }
   }