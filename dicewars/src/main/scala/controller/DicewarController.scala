package main.scala.controller
import scala.swing.event.Event
import scala.swing.Publisher
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
		  case Notification.Position=> println("Notify Position" + notification.position.column + notification.position.row)
		  case Notification.Reinforcement=>delegateReinforcement(notification)
		  case Notification.Battle=>delegateBattle(notification)
		  case _ => println("Falsche Notification")
	   }
    }
    
    def delegateBattle(notification:Notification)
    {
      game.setAttackAndDefenseLand(notification.currentPlayer, notification.position, notification.isOwnLand)
      //game.checkSelection(notification.currentPlayer, notification.position)
       
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
      }
    }
    
    
    def delegateReinforcement(notification:Notification)
    {
      game.handleReinforcement(notification.currentPlayer, notification.position)
    }

}