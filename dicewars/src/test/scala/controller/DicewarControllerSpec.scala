package test.scala.controller

import main.scala.model._
import main.scala.controller.DicewarController
import main.scala.view._
import main.scala.util._
import org.specs2.mutable._
import main.scala.controller.DicewarController

class FakeGame extends Gamefield
{
	var observer:DicewarController = null
	def addObserver(controller:DicewarController) =  observer = controller
}

class FakeView extends View
{
	var started = false
	var stopped = false
	var observer:DicewarController = null 
//	var displayMapMenu = false
//	var displayField = false
//	var displayMessage = false
//	var inputRequest = false
//	var questRequest = false
//	var moveRequest = false
//	var playerInitRequest = false
//	var exitRequest = false
	
	
	override def closeView = stopped = true
	
	override def startView = started = true
	
	def addObserver(controller:DicewarController) =  observer = controller
	override def updateObserver(notification:Notification) =
	{}
//	
//	override def updateObserver(notification:Notification) =
//	{
//	   notification.typ match
//	   {
//	     case Notification.MapSample =>  displayMapMenu = true
//	     case Notification.Input => inputRequest = true
//	     case Notification.Move => moveRequest = true
//	     case Notification.Question => questRequest = true
//	     case Notification.PlayerInit => playerInitRequest = true
//	     case Notification.Message => displayMessage = true 
//	     case Notification.DrawUI => displayField = true
//	     case Notification.Exit => exitRequest = true
//	     case _ => //println("Debug TUI: Falsche Notification")
//	   } 
//	}
}

class DicewarControllerSpec extends Specification
{
	def createTestSetup : (FakeGame, FakeView, DicewarController) =
	{
		val fakeGame = new FakeGame
		val fakeView = new FakeView
		val controller = new DicewarController(fakeGame)
		(fakeGame, fakeView, controller)
	}
	
	"A controller" should {
	  
	  "be able to join a view" in 
	  {
	    val (game, view, controller) = createTestSetup
	    game.observer mustEqual null
	    game.addObserver(view)
	    game.observer mustEqual this
	    view.observer mustEqual null
		view.addObserver(controller)
		view.observer mustEqual this
		view.started must beFalse
		view.startView
		view.started must beTrue
	  }
	  
	  "be able to detach a view" in 
	  {
	    val (game, view, controller) = createTestSetup
	    game.observer mustEqual null
	    game.addObserver(view)
	    game.observer mustEqual this
	    
	    controller.delegateExit
		
	    game.observer mustEqual this
	    game.removeObserver(view)
		game.observer mustEqual null
		
		view.stopped must beFalse
		view.closeView
		view.stopped must beTrue
	  }
	}
}