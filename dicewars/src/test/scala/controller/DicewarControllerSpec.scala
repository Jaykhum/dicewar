package test.scala.controller

import main.scala.model._
import main.scala.controller.DicewarController
import main.scala.view._
import main.scala.util._
import org.specs2.mutable._
import main.scala.controller.DicewarController

class FakeGame extends Gamefield
{
	var startGameHandling = false
	//override var currentInputType = "no String"
	var inputTypeValidation = false
	//override var currentPhase = 0
	var userAnswer = false
	var startSendMessage = false
	var messageContent:String = "no String"
	var selectedMap:String = "no String"
	var gameInitDone = false
	var startShowMapMenu =  false
	var choosenAmount = 0
	var startUnitMove = false
	var startReset = false
	var playerConfigOk = false
	var playerCount = 0
	var botCount = 0
	var doInitPlayer =false
	var posConstrainChecked = false
	var landSelectDone = false
	
	override def gameHandler = {startGameHandling = true}
	
	override def checkInputTypeValidation(inputType:String):Boolean = 
	{
	  if(currentInputType == inputType)
	  {
		  inputTypeValidation =  true
		  true
	  }
	  else
	  {
		  inputTypeValidation =  false
		  false
	  }
	}
	
	override def questionResponse(answer:Boolean) = {userAnswer = answer}
	
	override def sendNotificationMessage(messageType:Message.MessageTyp, messageContent:String) = 
	{
	  this.messageContent = messageContent
	  startSendMessage = true
	}
	
	override def initGame(map:String) =
	{
	  selectedMap = map
	  gameInitDone = true
	}
	
	override def startShowMapSelectionMenu = {startShowMapMenu = true}
	override def manageUnitMove(amount:Int) =
	{
	  startUnitMove = true
	  choosenAmount = amount
	}
	
	override def initPhase = {startReset = true}
	
	override def sendPlayerConfigMessage(playerCount:Int, botCount:Int):Boolean = 
	{
		if(playerCount > botCount)
		{
		  this.playerCount = playerCount
		  this.botCount =  botCount
		  true
		}
		else
		{
		  false
		}
	}
	
	override def initPlayer(humanPlayer:Integer, botPlayer:Integer):Array[Avatar] =
	{
	  doInitPlayer = true
	   val avatarContainer = new Array[Avatar](humanPlayer + botPlayer)
	  avatarContainer
	}
	
	override def checkPositionConstrains(pos:WorldPosition): Boolean =
	{
	  if(pos == null)
	  {
	    posConstrainChecked = false
	    false
	  }
	  else
	  {
	    posConstrainChecked = true
		true
	  }
	}
	
	override def setLandSelection(pos:WorldPosition) = { landSelectDone = true}
}



class FakeView extends View
{
	var started = false
	var stopped = false
	
	override def closeView = stopped = true
	override def startView = started = true
	override def updateObserver(notification:Notification){}
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
	    view.started must beFalse
	    game.subscriberContainer.isEmpty must beTrue
	    view.subscriberContainer.isEmpty must beTrue
		controller.joinView(view)
		game.subscriberContainer.head must be_==(view)
		view.subscriberContainer.head must be_==(controller)
		view.started must beTrue
	  }
	  
	  "be able to detach a view" in 
	  {
	    val (game, view, controller) = createTestSetup
		game.removeObserver(view)
		game.subscriberContainer.isEmpty must beTrue
		view.stopped must beFalse
		view.closeView
		view.stopped must beTrue
	  }
	  
	  "be able to start a game" in
	  {
	    val (game, view, controller) = createTestSetup
	    game.startGameHandling must beFalse
	    controller.startGame
	    game.startGameHandling must beTrue
	  }
	  
	  "be able to delegate an answer yes" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =  new Notification(Notification.Answer)
	    notification.answer = true
	    notification.inputType = "question"
	    game.currentPhase = 3
	    game.currentInputType = "question"
	      
	    controller.delegateAnswer(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beFalse
	    game.userAnswer must beTrue
	  }
	  
	  "be able to delegate an answer no" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =  new Notification(Notification.Answer)
	    notification.answer = false
	    notification.inputType = "question"
	    
	    game.currentPhase = 3
	    game.currentInputType = "question"
	    
	    controller.delegateAnswer(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beFalse
	    game.userAnswer must beFalse
	  }
	  
	  "be fail to delegate because wrong userinput type" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =  new Notification(Notification.Answer)
	    notification.answer = false
	    notification.inputType = "something"
	    game.currentPhase = 5
	    game.currentInputType = "question"
	      
	    controller.delegateAnswer(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(4)
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	  }
	  
	  "be fail to delegate because wrong type and phase 3" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =  new Notification(Notification.Answer)
	    notification.answer = false
	    notification.inputType = "question"
	    
	    game.currentPhase = 3
	    game.currentInputType ="something"
	    
	    controller.delegateAnswer(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(3)
	    game.startSendMessage must beTrue
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	  }
	  
	  "be able to delegate mapselection" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =new Notification(Notification.Map)
	    notification.map = "basicland"
	    notification.inputType = "map"
	    game.currentInputType ="map"
	    game.gameInitDone must beFalse  
	    
	    controller.delegateMapSelection(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.selectedMap must be_==("basicland")
	    game.gameInitDone must beTrue
	    game.currentPhase must be_==(1)
	    game.startGameHandling must beTrue
	    
	  }
	  
	  "be fail to delegate mapselection because of wrong input type" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =new Notification(Notification.Map)
	    notification.map = "basicland"
	    notification.inputType = "map"
	    game.currentInputType ="something else"
	      
	    game.gameInitDone must beFalse  
	    
	    controller.delegateMapSelection(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.currentPhase must be_==(0)
	    game.startGameHandling must beTrue
	    game.startSendMessage must beTrue
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	  }

	  "be able to delegate showMapSelectionMenu request" in
	  {
	    val (game, view, controller) = createTestSetup
	    controller.delegateMapSelectionMenu
	    game.startShowMapMenu must beTrue
	  }
	  
	  "be able to delegate army move amount" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Move)
	    notification.amount = 10
	    notification.inputType = "amount"
	    game.currentInputType ="amount"
	    
	    controller.delegateMove(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startUnitMove must beTrue
	    game.choosenAmount must be_==(10)
	  }
	  
	  "be fail to delegate army move amount because of input type" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Move)
	    notification.amount = 10
	    notification.inputType = "amount"
	    game.currentInputType ="something else"
	      
	    controller.delegateMove(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startUnitMove must beFalse
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(-1)
	  }
	  
	  "be fail to delegate army move amount because of input type and phase 3" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Move)
	    notification.amount = 10
	    notification.inputType = "amount"
	    game.currentInputType ="something else"
	    game.currentPhase = 3
	    
	    controller.delegateMove(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startUnitMove must beFalse
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(3)
	  }
	  
	  "be fail to delegate reset game request" in
	  {
	    val (game, view, controller) = createTestSetup
	    game.startReset must beFalse
	    
	    controller.delegateReset
	    
	    game.startReset must beTrue
	    
	  }
	  
	  "be able to delegate player init. with correct input" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.PlayerInit)
	    notification.playerCount = 2
	    notification.botCount = 1
	    notification.inputType = "playerInit"
	    game.currentInputType = "playerInit"
	    game.avatarContainer = new Array[Avatar](1)
	    
	    controller.delegatePlayerInit(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(1)
	    game.doInitPlayer must beTrue
	  }
	  
	  "be fail to delegate player init. because worong counts" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.PlayerInit)
	    notification.playerCount = 3
	    notification.botCount = 6
	    notification.inputType = "playerInit"
	    game.currentInputType = "playerInit"
	    game.avatarContainer = new Array[Avatar](1)
	    
	    controller.delegatePlayerInit(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(0)
	    game.doInitPlayer must beFalse
	  }
	  
	  "be fail to delegate player init. because worong input type" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.PlayerInit)
	    notification.playerCount = 3
	    notification.botCount = 6
	    notification.inputType = "playerInit"
	    game.currentInputType = "something else"
	    game.avatarContainer = new Array[Avatar](1)
	    
	    controller.delegatePlayerInit(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(0)
	    game.doInitPlayer must beFalse
	    game.messageContent must be_==("Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	  }

	  
	  "be able to delegate Position" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = new WorldPosition(5, 2)
	    notification.inputType = "position"
	    game.currentInputType = "position"
	      
	    controller.delegatePosition(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.posConstrainChecked must beTrue
	    game.landSelectDone must beTrue
	  }
	  
	  "be fail to delegate Position because wrong type" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = new WorldPosition(5, 2)
	    notification.inputType = "position"
	    game.currentInputType = "something else"
	      
	    controller.delegatePosition(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startSendMessage must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(-1)
	  }
	  
	  "be fail to delegate Position because wrong pos" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = null
	    notification.inputType = "position"
	    game.currentInputType = "position"
	      
	    controller.delegatePosition(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.posConstrainChecked must beFalse
	    game.startSendMessage must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(-1)
	  }
	  
	  "be fail to delegate Position because wrong pos and phase 8" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = null
	    notification.inputType = "position"
	    game.currentInputType = "position"
	    game.currentPhase = 8
	    
	    controller.delegatePosition(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.posConstrainChecked must beFalse
	    game.startSendMessage must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(8)
	  }
	  
	  "be fail to delegate Position because wrong type and phase 3" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = new WorldPosition(5, 2)
	    notification.inputType = "position"
	    game.currentInputType = "something else"
	    game.currentPhase = 3
	    
	    controller.delegatePlayerInit(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startSendMessage must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(3)
	  }
	  
	  "be fail to delegate Position because wrong type and phase 20" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = new WorldPosition(5, 2)
	    notification.inputType = "position"
	    game.currentInputType = "something else"
	    game.currentPhase = 20
	    
	    controller.delegatePlayerInit(notification)
	    
	    game.inputTypeValidation must beFalse
	    game.startSendMessage must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(20)
	  }
	
	  "be able to react to notification MapSample" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.MapSample)
	    
	    controller.updateObserver(notification)
	    
	    game.startShowMapMenu must beTrue
	  }
	  
	  "be able to react to notification Reset" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Reset)
	    
	    controller.updateObserver(notification)
	    
	    game.startReset must beTrue
	  }
	  
	  "be able to react to notification Map" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =new Notification(Notification.Map)
	    notification.map = "basicland"
	    notification.inputType = "map"
	    game.currentInputType ="map"
	      
	    controller.updateObserver(notification)
	    
	    game.selectedMap must be_==("basicland")
	    game.gameInitDone must beTrue
	  }
	  
	  "be able to react to notification Position" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.Position)
	    notification.position = new WorldPosition(5, 2)
	    notification.inputType = "position"
	    game.currentInputType = "position"
	      
	    controller.updateObserver(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.posConstrainChecked must beTrue
	    game.landSelectDone must beTrue
	  }
	  
	  "be able to react to notification Move" in
	  {
	    val (game, view, controller) = createTestSetup
	  	var notification = new Notification(Notification.Move)
	    notification.amount = 10
	    notification.inputType = "amount"
	    game.currentInputType ="amount"
	    
	    controller.updateObserver(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startUnitMove must beTrue
	    game.choosenAmount must be_==(10)
	  }
	  
	  "be able to react to notification Answer" in
	  {
	    val (game, view, controller) = createTestSetup
	    val notification =  new Notification(Notification.Answer)
	    notification.answer = true
	    notification.inputType = "question"
	    game.currentPhase = 3
	    game.currentInputType = "question"
	      
	    controller.updateObserver(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beFalse
	    game.userAnswer must beTrue
	  }
	  
	  "be able to react to notification PlayerInit" in
	  {
	    val (game, view, controller) = createTestSetup
	    var notification = new Notification(Notification.PlayerInit)
	    notification.playerCount = 2
	    notification.botCount = 1
	    notification.inputType = "playerInit"
	    game.currentInputType = "playerInit"
	    game.avatarContainer = new Array[Avatar](1)
	    
	    controller.updateObserver(notification)
	    
	    game.inputTypeValidation must beTrue
	    game.startGameHandling must beTrue
	    game.currentPhase must be_==(1)
	    game.doInitPlayer must beTrue
	  }
	}
}