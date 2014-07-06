package test.scala.model
import org.specs2.mutable._
import main.scala.model._
import main.scala.view._
import main.scala.controller._
import main.scala.util._

class GamefieldSpec extends Specification {
  
  

	class TestView extends View
	{	
	  
	  var messageContent:String = ""
	  var isCallDrawUI:Boolean = false
	  var isMapSample = false
	  var isInput = false
	  var isMove = false
	  var isQuestion = false
	  var isPlayerInit = false
	  var isExit = false
	  var isGameOver = false
		override def updateObserver(notification:Notification)
		{
		    notification.typ match
			   {
			   	case Notification.MapSample =>  isMapSample = true
		      	case Notification.Input => isInput = true
		     	case Notification.Move => isMove = true
		     	case Notification.Question => isQuestion = true
		     	case Notification.PlayerInit => isPlayerInit = true
		     	case Notification.Exit => isExit = true
				case Notification.Message =>  messageContent = notification.message.content
			    case Notification.DrawUI =>  isCallDrawUI = true
			    case Notification.GameOver => isGameOver = true
			   }
		}
	  
	  override def closeView {}
	  override def startView {}
	  
	  
	  
	}
  
  	/**
  	 * 
  	 */
	def createTestSetupGame:(Gamefield,TestView) =
	{
		val testGame:Gamefield = new Gamefield
		val testView = new TestView
		testGame.addObserver(testView)
      
		val testplayerContainer = Array[Avatar](new Avatar(0), new Avatar(1), new Bot(2, testGame))
		testplayerContainer(0).color = Avatar.colorContainer(0) 
		testplayerContainer(1).color = Avatar.colorContainer(1)  
		testplayerContainer(2).color = Avatar.colorContainer(2)
		
		testGame.avatarContainer = testplayerContainer
		testGame.currentPlayer = testGame.avatarContainer(0)
		
		(testGame,testView)
	}
	
	def createFieldAndHolderSetup:(Gamefield,TestView) =
	{
	  val testGame:Gamefield = new Gamefield
		val testView = new TestView
		testGame.addObserver(testView)
      
		val testplayerContainer = Array[Avatar](new Avatar(0), new Avatar(1), new Bot(2, testGame))
		testplayerContainer(0).color = Avatar.colorContainer(0) 
		testplayerContainer(1).color = Avatar.colorContainer(1)  
		testplayerContainer(2).color = Avatar.colorContainer(2)
		
		testGame.avatarContainer = testplayerContainer
		testGame.currentPlayer = testGame.avatarContainer(0)
		
		for(i <- 0 until World.height; j <- 0 until World.width)
			testGame.world(i)(j) = new Water(i,j)
		
		var holder = 0
		var counter = 0
		val testFieldContainer = new Array[Field](12)
		for(i <- 4 to 5; j <- 7 to 12)
		{
		  var createdTestField = new Field(i,j)
		  testGame.world(i)(j) = createdTestField
		  testFieldContainer(counter) = createdTestField
		  testGame.world(i)(j).setArmy(3)
		  testGame.world(i)(j).setHolder(testGame.avatarContainer(holder).id)
		  holder += 1
		  counter += 1
		  if(holder > 2)
			  holder = 0
		}

	  	testGame.fieldContainer = testFieldContainer
		
		
		(testGame,testView)
	}
  

  "A Gamefield" should {
    
    "be able to initialise Phase" in {
      val testGame:Gamefield = new Gamefield
    	testGame.initPhase
		testGame.fromLand must beNull
		testGame.toLand must beNull
		testGame.gameOver must beFalse 
		testGame.currentPhase mustEqual 0
		testGame.fieldContainer must beNull
		testGame.moveOption mustEqual 1
		testGame.currentPlayer must beNull
		testGame.currentInputType mustEqual "playerInit"
		testGame.currentRequestedPositionType mustEqual 0
    }

    
    "be able to initalise Player" in {
      val testGame:Gamefield = new Gamefield
      
      val testplayerContainer = Array[Avatar](new Avatar(0), new Avatar(1), new Bot(2, testGame))
      testplayerContainer(0).color = Avatar.colorContainer(0) 
      testplayerContainer(1).color = Avatar.colorContainer(1)  
      testplayerContainer(2).color = Avatar.colorContainer(2)
      
      val testGameInitPlayer = testGame.initPlayer(2, 1)
      testGameInitPlayer.length  mustEqual  testplayerContainer.length
      testGameInitPlayer(2) must haveClass[Bot]
      testGame.avatarContainer must not be null
    }
    
    
     "be able to set Water in the whole World" in {
      val testGame:Gamefield = new Gamefield
      testGame.initWorld
      var testList = for(i <- 0 until World.height; j <- 0 until World.width)yield testGame.world(i)(j).getFieldType
      testList.forall(isField => !isField ) must beTrue      
    }
     
     
     
     "be able to initialise the game" in {
       val (testGame,testView) = createTestSetupGame
       testGame.initGame("basicland")
       testGame.fieldContainer must not be null
       testGame.fieldContainer must have size(8)
       testGame.fieldContainer.forall(field => field.getHolder != -3) must beTrue
     }
     
     "be able to attack" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPhase = 9 
       testGame.attack
       testGame.currentInputType mustEqual "question"
       testGame.currentPhase must be_==(10)
       testView.messageContent mustEqual "Angreifen? (ja/nein)"
       
     }
     
     "be able to attack an other land" in{

       val (testGame,testView) = createFieldAndHolderSetup
       testGame.attackAnOtherLand
       testGame.currentInputType mustEqual "question"
       testGame.currentPhase must be_==(16)
       testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)"
       
       testGame.currentPlayer = testGame.avatarContainer(0)
       val tempOwnLand = testGame.fieldContainer.filter(field => field.getHolder == testGame.avatarContainer(0).id)
       tempOwnLand.foreach(land => land.setArmy(1))
       testGame.attackAnOtherLand
       testGame.currentPhase must be_==(3)
       
     }
     
     
     "be able to continue to attack an otherland" in{
       val (testGame,testView) = createFieldAndHolderSetup   
       testGame.currentPlayer.answer = true
       testGame.attackAnOtherLandContinue
       testGame.currentPhase must be_==(7)
	   testGame.fromLand must beNull
	   testGame.toLand must beNull
     }
     
     "be able to stop to attack an otherland" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPlayer.answer = false
       testGame.attackAnOtherLandContinue
       testGame.currentPhase must be_==(18)
	   testGame.fromLand must beNull
	   testGame.toLand must beNull
     }
     
      "be able to attack again a land" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.toLand = testGame.fieldContainer(3)
       testGame.fromLand.setArmy(1)
       testGame.currentPlayer.answer = true
       testGame.currentPhase = 12
       testGame.attackContinue
       testGame.currentPhase must be_==(13)
     }
      
      "be able to stop attack again a land" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPlayer.answer = false
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.toLand = testGame.fieldContainer(3)
       testGame.currentPhase = 12
       testGame.attackContinue
       testGame.currentPhase must be_==(16)
	   testGame.fromLand must beNull
	   testGame.toLand must beNull
	   testView.isCallDrawUI must beTrue
	   testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)"
     }
      
      "be able to handle an attack" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.toLand = testGame.world(0)(0)
       testGame.currentPlayer.answer = true
       testGame.currentPhase = 12
       testGame.attackContinue
       testGame.currentPhase must be_==(11)
       testView.messageContent mustEqual "Ist kein Feld sondern Wasser"
     }
      
      "be able to ask to attack the same land" in{
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.toLand = testGame.fieldContainer(3)
       testGame.attackSameLand
       testGame.currentPhase must be_==(13)
	   testGame.currentInputType mustEqual "question"
	   testView.messageContent mustEqual "Weiter angreifen? (ja/nein)"
     }
      
      "be able to inform about an lost attack" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.fromLand.setArmy(1)
       testGame.currentPlayer.answer = false
       testGame.attackSameLand
       testGame.currentPhase must be_==(16)
       testGame.currentInputType mustEqual "question"
       testView.isQuestion = true
       testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)"
      }
      
       "be able to handle a continue attack of the same Land" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.fieldContainer(2)
       testGame.toLand = testGame.fieldContainer(3)
       testGame.currentPlayer.answer = false
       testGame.attackSameLandContinue
       testGame.currentPhase must be_==(16)
       testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)"
      }
       
       
       "be able to ask of a enemy land selection for a battle" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.battlePhaseChooseEnemyLand
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Waehle das Land, welches du angreifen willst."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(2)
      }
     
       "be able to ask of a own land selection for a battle" in {
       val (testGame,testView) = createFieldAndHolderSetup
       for(i <- 0 until testGame.fieldContainer.length)
       testGame.fieldContainer(i).setArmy(1)
       testGame.battlePhaseChooseOwnLand
       testGame.currentPhase must be_==(3)
       testView.messageContent mustEqual "Bitte Teritorium eingeben (Spalte,Zeile)."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       testGame.fromLand must beNull
	   testGame.toLand must beNull
      }
       
      "be able to execute a single attack" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.world(0)(0)
       testGame.toLand = testGame.world(1)(1)
       testGame.singleAttack(testGame.world(0)(0), testGame.world(1)(1))
       testView.messageContent mustEqual "Ist kein Feld sondern Wasser"
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(7)
       testGame.singleAttack(testGame.world(4)(7), testGame.world(4)(8))
       testGame.gameOver must beFalse
       
       testGame.world(4)(9).setArmy(10)
       testGame.world(4)(10).setArmy(1)
       testGame.fromLand = testGame.world(4)(9)
       testGame.toLand = testGame.world(4)(10)
       testGame.singleAttack(testGame.world(4)(9), testGame.world(4)(10))
       testGame.gameOver must beFalse
       
       testGame.world(4)(11).setArmy(2)
       testGame.world(4)(12).setArmy(13)
       testGame.fromLand = testGame.world(4)(11)
       testGame.toLand = testGame.world(4)(12)
       testGame.singleAttack(testGame.world(4)(11), testGame.world(4)(12))
       testGame.gameOver must beFalse
       
       testGame.world(5)(8).setArmy(8)
       testGame.world(5)(9).setArmy(3)
       testGame.fromLand = testGame.world(5)(8)
       testGame.toLand = testGame.world(5)(9)
       testGame.singleAttack(testGame.world(5)(8), testGame.world(5)(9))
       testGame.gameOver must beFalse
       
      }
       
     "be able to start a battle" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.startBattle
       testGame.fromLand must beNull
	   testGame.toLand must beNull
       testGame.currentInputType mustEqual "question"
       testView.messageContent mustEqual "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)"
       testGame.currentPhase must be_==(6)
 
     }
     
     "be able to prohibit to start a battle" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.startBattle
       for(i <- 0 until testGame.fieldContainer.length)
    	   testGame.fieldContainer(i).setArmy(1)
       testGame.startBattle
       testGame.currentPhase must be_==(3)
       testView.messageContent mustEqual "Bitte Teritorium eingeben (Spalte,Zeile)."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       testGame.fromLand must beNull
	   testGame.toLand must beNull
     }
     
     "be able to set own land" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = null
       testGame.currentPhase = 3
       testGame.currentPlayer.newUnitsTemporary = 1     
       testGame.setOwnLand(new WorldPosition(4, 7))
       testGame.currentPhase must be_==(6)
       testGame.fromLand must beNull
	   testGame.toLand must beNull
       testGame.currentInputType mustEqual "question"
       testView.messageContent mustEqual "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)"
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.world(4)(7).setArmy(1)
       testGame.currentPhase = 7
       testGame.currentPlayer.newUnitsTemporary = 1     
       testGame.setOwnLand(new WorldPosition(4, 7))      
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Waehle das Land, von welchem du aus angreifen willst."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       
       testGame.fromLand = null
       testGame.world(4)(7).setArmy(1)
       testGame.currentPhase = 7
       testGame.currentPlayer.newUnitsTemporary = 1     
       testGame.setOwnLand(new WorldPosition(4, 7))      
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Waehle das Land, von welchem du aus angreifen willst."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       
       testGame.fromLand = null
       testGame.currentPhase = 7
       testGame.currentPlayer.newUnitsTemporary = 1     
       testGame.setOwnLand(new WorldPosition(4, 8))
       testGame.isWaterLand(new WorldPosition(4, 8)) must beFalse
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Waehle das Land, von welchem du aus angreifen willst."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)       
       
     }
     
     "be able to set enemy land" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.world(4)(7)
       testGame.currentPhase = 8
       testGame.setEnemyLand(new WorldPosition(4, 7))      
       testGame.fromLand must beNull
	   testGame.toLand must beNull
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Waehle das Land, von welchem du aus angreifen willst."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       testGame.currentPhase must be_==(7)
       
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.currentPhase = 8
       testGame.setEnemyLand(new WorldPosition(4, 8))
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "question"
       testGame.currentPhase must be_==(10)
       testView.messageContent mustEqual "Angreifen? (ja/nein)"
     }
     
     "be able to set second own land" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.world(4)(7)
       testGame.currentPhase = 20
       testGame.setOwn2Land(new WorldPosition(4, 7))
       testGame.fromLand must beNull
	   testGame.toLand must beNull
       testView.isCallDrawUI must beTrue
       testView.messageContent mustEqual "Bitte waehle ein eigenes Land aus um Truppen zu verschieben"
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       testGame.currentPhase must be_==(19)
       
   
       testGame.fromLand = testGame.world(4)(7)
       testGame.currentPhase = 20
       testGame.setOwn2Land(new WorldPosition(4, 8))      
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "position"
       testGame.currentPhase must be_==(20)
       testGame.currentRequestedPositionType must be_==(3)
       testView.messageContent mustEqual "Bitte waehle ein weiteres eigenes Land aus um Truppen darauf zu verschieben"
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = null
       testGame.currentPhase = 20
       testGame.setOwn2Land(new WorldPosition(5, 7))
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(2)
       testGame.currentPhase must be_==(21)
       testView.messageContent mustEqual  "Hinweis: Eine Einheit muss stationiert bleiben."
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = null
       testGame.currentPhase = 20
       testGame.setOwn2Land(new WorldPosition(4, 10))
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "position"
       testGame.currentPhase must be_==(20)
       testGame.currentRequestedPositionType must be_==(3)
       testView.messageContent mustEqual  "Bitte waehle ein weiteres eigenes Land aus um Truppen darauf zu verschieben"
       
       
     }
     
     
     "be able to set land selection" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentRequestedPositionType = 1
       testGame.fromLand = null
       testGame.currentPhase = 3
       testGame.currentPlayer.newUnitsTemporary = 1     
       testGame.setLandSelection(new WorldPosition(4, 7))
       testGame.currentPhase must be_==(6)
       testGame.fromLand must beNull
	   testGame.toLand must beNull
       testGame.currentInputType mustEqual "question"
       testView.messageContent mustEqual "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)"
       
       testGame.currentRequestedPositionType = 2
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = null
       testGame.currentPhase = 8
       testGame.setLandSelection(new WorldPosition(4, 8))
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "question"
       testGame.currentPhase must be_==(10)
       testView.messageContent mustEqual "Angreifen? (ja/nein)"
       
       
       testGame.currentRequestedPositionType = 3      
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = null
       testGame.currentPhase = 20
       testGame.setLandSelection(new WorldPosition(5, 7))
       testView.isCallDrawUI must beTrue
       testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(2)
       testGame.currentPhase must be_==(21)
       testView.messageContent mustEqual  "Hinweis: Eine Einheit muss stationiert bleiben."
     }
     
     "be able to handle a response of a question" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPhase = 0
       testGame.questionResponse(true)
       testGame.currentInputType mustEqual  "playerInit"     
       
     }
     
     "be able to check gameOver" in {
       val (testGame,testView) = createFieldAndHolderSetup       
       testGame.checkGameOver must beFalse    
       testGame.avatarContainer.foreach(player => player.lost = true)
       testGame.currentPlayer.lost = false
       testGame.checkGameOver must beTrue
     }
     
     "be able to check the requested number of move" in {
       val (testGame,testView) = createFieldAndHolderSetup
       
       testGame.currentPlayer.minimumMove = 3
       
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.checkNumberOfUnitMove(3) must beFalse
       testView.messageContent mustEqual  "Zuviele Einheiten gewaehlt. Bitte erneut eingeben."
       
       
       testGame.world(4)(7).setArmy(6)
       testGame.fromLand = testGame.world(4)(7)   
       testGame.currentPlayer.minimumMove = 3
       testGame.checkNumberOfUnitMove(2) must beFalse
       testView.messageContent mustEqual  "Es muessen mindestens " + testGame.currentPlayer.minimumMove + "  Einheiten verschoben werden, da mit sovielen Einheiten auch angegriffen wurde. Bitte erneut eingeben."
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.currentPlayer.minimumMove = 0
       testGame.checkNumberOfUnitMove(0) must beFalse
       testView.messageContent mustEqual  "Zu wenige Einheiten die verschoben werden sollen. Bitte erneut eingeben."

      
       testGame.fromLand = testGame.world(4)(7)
       testGame.fromLand.setArmy(6)
       testGame.checkNumberOfUnitMove(3) must beTrue
       
     }
     
     "be able to check if player is out of game" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPlayer.occupiedTerritory == 0
       testGame.checkPlayerOutOfGame(testGame.currentPlayer.id) must beTrue
       
     }
     
     "be able to check position constraints" in {
       val (testGame,testView) = createFieldAndHolderSetup
       
       testGame.checkPositionConstrains(new WorldPosition(0,20)) must beFalse
       testView.messageContent mustEqual  "Das Ausgewaehle Land befindet sich nicht auf dieser Welt."
       
       testGame.checkPositionConstrains(new WorldPosition(0,0)) must beFalse
       testView.messageContent mustEqual  "Ein Wasserfeld hat kein besitzer"
       
       testGame.checkPositionConstrains(new WorldPosition(4,7)) must beTrue
     }
     
     "be able to validate inpute type" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentInputType = "position"
       testGame.checkInputTypeValidation("position") must beTrue
       
        testGame.checkInputTypeValidation("amount") must beFalse
     }
     
     "be able to check a land selection for battle" in {
        val (testGame,testView) = createFieldAndHolderSetup
        
        testGame.checkBattleLandSelection(testGame.currentPlayer,new WorldPosition(4,7) ,true) must beTrue
       testGame.checkBattleLandSelection(testGame.currentPlayer,new WorldPosition(4,8) ,true) must beFalse
       
       testGame.fromLand = testGame.world(4)(7)
       testGame.checkBattleLandSelection(testGame.currentPlayer,new WorldPosition(4,8) ,false) must beTrue
       testGame.checkBattleLandSelection(testGame.currentPlayer,new WorldPosition(4,7) ,false) must beFalse
       
       testGame.world(4)(8).setHolder(testGame.currentPlayer.id)
       testGame.checkBattleLandSelection(testGame.currentPlayer,new WorldPosition(4,7) ,true) must beFalse
       
     }
     
     "be able to check enemy land selection" in {
        val (testGame,testView) = createFieldAndHolderSetup
        
        testGame.fromLand = testGame.world(4)(7)
        
        testGame.checkEnemyLandSelection(testGame.currentPlayer,new WorldPosition(4,9)) must beFalse
        testView.messageContent mustEqual  "Das ausgewaehlte Land ist kein Nachbarland."
        
        testGame.checkEnemyLandSelection(testGame.currentPlayer,new WorldPosition(5,7)) must beFalse
        testView.messageContent mustEqual  "Das ausgewaehlte Land ist ein eigenes Teritorium."
        
        testGame.checkEnemyLandSelection(testGame.currentPlayer,new WorldPosition(4,6)) must beFalse
        testView.messageContent mustEqual  "Ein Wasserfeld hat kein besitzer"

     }
     
     "be able to get Avatar" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.getAvatar(10)
       testView.messageContent mustEqual  ""
     }
     
//     "be able to exit" in {
//       val (testGame,testView) = createFieldAndHolderSetup
//       testGame.exitGame
//       testView.isExit must beTrue
//     }
     
     "be able to send gameover notification" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.sendNotificationGameOver(testGame.currentPlayer)
       testView.isGameOver must beTrue
       testView.messageContent mustEqual  " hat Gewonnen !!!"
     }
     
     "be able to handle move" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8)
       testGame.moveHandler
       testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(1)
     }
     
     "be able to move after battle" in {
       val (testGame,testView) = createFieldAndHolderSetup
       
       testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8)
       testGame.battleMoveUnit(testGame.world(4)(7), testGame.world(4)(8), 3)
       testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(1)
       testGame.currentPhase must be_==(14)
       testView.messageContent mustEqual ":  Noch verbliebene Einheiten: " + testGame.fromLand.getArmy 
       
       testGame.world(4)(7).setArmy(2)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8)
       testGame.battleMoveUnit(testGame.world(4)(7), testGame.world(4)(8), 2)
       testGame.currentPhase must be_==(16)
     }
     
     "be able to activate bot move" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.currentPlayer = testGame.avatarContainer(2)
       testGame.activateBotMove
       testGame.currentPhase must be_==(3)
       testView.messageContent mustEqual "Bitte Teritorium eingeben (Spalte,Zeile)."
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(1)
       testGame.fromLand must beNull
	   testGame.toLand must beNull
	   
     }
     
     "be able to set player count" in {
       val (testGame,testView) =  createTestSetupGame
       
      testGame.sendPlayerConfigMessage(2,1) must beTrue
      testGame.sendPlayerConfigMessage(3,1) must beFalse
      testView.messageContent mustEqual "Es koennen insgesamt hoechsten drei mitspielen."
      
      testGame.sendPlayerConfigMessage(0,2) must beFalse
      testView.messageContent mustEqual "Es wird mindestens ein Spieler benoetigt. " 
       
     }
     
     "be able to move option one" in {
       val (testGame,testView) = createFieldAndHolderSetup
       
       testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8)   
       testGame.currentPlayer.minimumMove = 3
       testGame.currentPhase = 14
       testGame.moveOptionOne(4)
       testGame.currentPhase must be_==(16)
	   testView.isCallDrawUI must beTrue
	   testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)"
	   
	   
	   testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8) 
       testGame.currentPlayer.minimumMove = 3
       testGame.currentPhase = 14
       testGame.moveOptionOne(2)
	   testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(1)
       testGame.currentPhase must be_==(14)
       testView.messageContent mustEqual ":  Noch verbliebene Einheiten: " + testGame.fromLand.getArmy 
       
     }
     
     "be able to move option two" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(5)(7)
       testGame.currentPhase = 20
       testGame.currentPlayer.minimumMove = 3
       testGame.moveOptionTwo(4)
       testGame.currentInputType mustEqual "amount"
       testGame.moveOption must be_==(2)
       testGame.currentPhase must be_==(21)
       testView.messageContent mustEqual "Hinweis: Eine Einheit muss stationiert bleiben."
     }
     
      "be able to send exit" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.sendExit
       testView.isExit must beTrue
      }
      
      "be able to display samples" in {
       val (testGame,testView) = createFieldAndHolderSetup
       testGame.startShowMapSelectionMenu
       testView.isMapSample must beTrue
      }
     
      "be able to manage unit move" in {
        val (testGame,testView) = createFieldAndHolderSetup
       
       testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(0)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(4)(8)   
       testGame.currentPlayer.minimumMove = 3
       testGame.currentPhase = 14
       testGame.moveOption = 1
       testGame.manageUnitMove(4)
       testGame.currentPhase must be_==(16)
	   testView.isCallDrawUI must beTrue
	   testView.messageContent mustEqual "Moechtest du ein anderes Land Angreifen? (ja/nein)" 
        
        
       testGame.world(4)(7).setArmy(6)
       testGame.world(4)(8).setArmy(1)
       testGame.fromLand = testGame.world(4)(7)
       testGame.toLand = testGame.world(5)(7)
       testGame.currentPhase = 20
       testGame.currentPlayer.minimumMove = 3
       testGame.moveOption = 2
       testGame.manageUnitMove(7)
       testGame.currentPhase must be_==(20)
       testGame.currentInputType mustEqual "position"
       testGame.currentRequestedPositionType must be_==(3)
             
       testView.isCallDrawUI must beTrue
	   testView.messageContent mustEqual "Bitte waehle ein weiteres eigenes Land aus um Truppen darauf zu verschieben"      
       
      }
      
      "be able to continue a battle" in {
        val (testGame,testView) = createFieldAndHolderSetup
        testGame.currentPlayer.answer = true
        testGame.battlePhaseContinue
        testGame.currentPhase must be_==(7)
        testGame.currentInputType mustEqual "position"
        
        testGame.currentPlayer.answer = false
        testGame.battlePhaseContinue
        testGame.currentPhase must be_==(18)
        testGame.currentInputType mustEqual "question"
        testView.messageContent mustEqual "Taktische Phase!\nMoechtest du Einheiten verschieben? (ja/nein)"
        
      }
      
       "be able to continue the tactic" in {
        val (testGame,testView) = createFieldAndHolderSetup
        testGame.currentPlayer.answer = true
        testGame.tacticPhaseContinue
        testGame.currentPhase must be_==(19)
        testGame.currentInputType mustEqual "position"
        testGame.currentRequestedPositionType must be_==(1)            
        testView.isCallDrawUI must beTrue
	    testView.messageContent mustEqual "Bitte waehle ein eigenes Land aus um Truppen zu verschieben"      
        
        testGame.currentPlayer.answer = false
        testGame.tacticPhaseContinue
        testGame.currentPhase must be_==(3)
        testView.messageContent mustEqual "Bitte Teritorium eingeben (Spalte,Zeile)."
        testGame.currentInputType mustEqual "position"
        testGame.currentRequestedPositionType must be_==(1)
        
      }
       
       "be able to handle the states" in {
         val (testGame,testView) = createFieldAndHolderSetup
         testGame.currentPhase = 1
         testGame.gameHandler
         testView.isMapSample must beTrue
         
         testGame.fromLand = testGame.world(4)(7)
         testGame.currentPlayer.newUnitsTemporary = 1
         testGame.currentPhase = 4
         testGame.gameHandler
         testView.isCallDrawUI must beTrue
         testGame.currentInputType mustEqual "question"
         testView.messageContent mustEqual "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)"      
         
       }
      

    
  }




}