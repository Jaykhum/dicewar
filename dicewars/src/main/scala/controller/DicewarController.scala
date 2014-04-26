package main.scala.controller
import main.scala.model._
import main.scala.view._
import main.scala.util._

class DicewarController extends Observer {
    val game =  new Gamefield
    game.initWorld
    val tui= new TUI(game)
    
    tui.addObserver(this)
    game.addObserver(tui)
    
    tui.startTUI
    
    startGamePhase;
    
    
    override def updateObserver(notification:Notification)
    {
     notification.typ match
	   {
		  case Notification.Map => initGame(notification)
		  case Notification.Reinforcement=>delegateReinforcement(notification)
		  case Notification.Battle=>delegateBattle(notification)
		  case Notification.Attack=>delegateAttack(notification)
		  case Notification.Question=>delegateQuestion(notification)
		  case Notification.Tactic=>delegateTactic(notification)
		  case Notification.Army=>delegateArmy(notification)
		  case _ => println("Falsche Notification")
	   }
    }
    
     def delegateArmy(n:Notification)
    {
       var from = n.currentPlayer.fromLand
       from.permissionMoveArmy = game.checkNumberOfUnitMove(from, n.value)
        if(from.permissionMoveArmy)
        n.currentPlayer.newUnitsTemporary = n.value
    }
    
    def delegateTactic(notification:Notification)
    {
      game.setFromOrTo(notification.currentPlayer, notification.position, notification.isFirstLand)
     
    }
    
    def delegateQuestion(notification:Notification)
    {
     //(avatarContainer )Moegliche Fehlerquelle durch id auf zugriff array index, funkt nur weil index und id synchron sind
     var player = game.avatarContainer(notification.currentPlayer.id) 
     player.myTurn = notification.question
    }
    
    def delegateAttack(notification:Notification)
    {
      game.fromLand.permissionMoveArmy = game.checkNumberOfUnitMove(game.fromLand, notification.value)
      game.setValueForAttackAndDefenseLand(notification.value)
         
    }
    
    def delegateBattle(notification:Notification)
    {
      game.setAttackOrDefenseLand(notification.currentPlayer, notification.position, notification.isOwnLand)

       
    }
    
    def initGame(notification:Notification)
    {
       game.initWorld
       game.initGame(notification.map)
       tui.showField
    }
    
    /**
     * Start the Reinforcement-, Battle- and Tactical-phase foreach player.
     */
    def startGamePhase
    {
      for(i <- 0 to game.avatarContainer.size -1)
      {
        game.startReinforcement(game.avatarContainer(i))
        game.startBattlePhase(game.avatarContainer(i))
        game.startTacticPhase(game.avatarContainer(i))
      }
    }
    
    
    def delegateReinforcement(notification:Notification)
    {
      game.handleReinforcement(notification.currentPlayer, notification.position)
    }

>>>>>>> branch 'master' of https://github.com/Jaykhum/dicewar.git
}
