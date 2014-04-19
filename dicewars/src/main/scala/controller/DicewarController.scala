package main.scala.controller
import scala.swing.event.Event
import scala.swing.Publisher
import main.scala.model._
import main.scala.view._
import main.scala.util._

class DicewarController extends Observer {
    val game =  new Gamefield
    game.initMap
    val tui= new TUI(game)
    
    tui.addObserver(this)
    game.addObserver(tui)
    
    tui.startTUI
    
    override def updateObserver(notification:Notification)
    {
     notification.typ match
	   {
		  case Notification.Map => game.mapPosition(notification.map)
		  case Notification.Position=> println("Notify Position" + notification.position.column + notification.position.row)
		  case _ => println("Falsche Notification")
	   }
    }

}