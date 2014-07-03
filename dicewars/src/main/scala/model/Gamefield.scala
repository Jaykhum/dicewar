package main.scala.model

// scala packages
import scala.collection.immutable.StringOps
import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.util.Random

// own costum packages
import main.scala.util._
import main.scala.util.FileUtil
import main.scala.util.Message

class Gamefield extends Observable
{
  	var avatarContainer:Array[Avatar] = null
  	var fieldContainer:Array[Field] = null
	
	var fromLand:Land = null
	var toLand:Land = null
	var gameOver:Boolean = false
	//phase e.g. meaning 0=> Init., 1 => Reinforcement, 2=>Battle, 3=>Tactic
	var currentPhase:Int = 0
	
	var moveOption:Int = 1
	
	// path to Maps
	val mapDir:String = "Maps/"
	
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](World.height,World.width)
	
	// current flags and states of the game
	var currentPlayer:Avatar = null
	var	currentInputType:String = ""
	var currentRequestedPositionType: Int = 0
  	
	// Init. the world with only water fields
	initWorld
	
	/*
	 * Initializing all parameters
	 * */
	def initPhase
	{
		fromLand = null
		toLand = null
		gameOver = false
		currentPhase = 0
		fieldContainer = null
		moveOption = 1
		currentPlayer = null
		currentInputType = "playerInit"
		currentRequestedPositionType = 0
		gameHandler
	}
	
  	
  	/*
  	 * Close this application
  	 * */
  	def exitGame
	{
	  System.exit(0)
	}
  	
	
  	/*
	 * Initialize all Fields and they appended holder.
	 * @ map: selected map setup
	 * */
	def initGame(map:String)
	{
	  fieldContainer = initFieldInWorld(map)
	  initFieldHolder(fieldContainer)
	}
  	
  	
  	/*
	 * Assigns player to created fields.
	 * @ fieldContaine:  contains required array with all fields
	 * */
	def initFieldHolder(fieldContainer:Array[Field])
	{
	  val playerCount = avatarContainer.size
	  val rnd = new Random(playerCount)
	  var tempFieldHolderID =  0
	  var playerFieldCount = new Array[Int](playerCount)
	  var fieldCount:Int = fieldContainer.size
	  for(i <- 0 until playerFieldCount.size)
		  playerFieldCount(i) = 0

	  for(i <- 0 until fieldContainer.size)
	  {
		 tempFieldHolderID =  rnd.nextInt(playerCount)
		 var chosePlayer = false
		 var nextPlayerCounter = 0
		 while(!chosePlayer)
		 {
			 if( (playerFieldCount(tempFieldHolderID)) < (fieldCount/playerCount) )
			  {
			    tempFieldHolderID
			    chosePlayer = true
			  }
			 else if((playerFieldCount(tempFieldHolderID)) >= (fieldCount/playerCount) && nextPlayerCounter != playerCount)
			 {
			   nextPlayerCounter += 1
			   tempFieldHolderID += 1
			   if(tempFieldHolderID == playerCount)
			   {
			     tempFieldHolderID = 0
			   }
			     
			 }
			else if((playerFieldCount(tempFieldHolderID)+1) >= (fieldCount/playerCount) && nextPlayerCounter == playerCount)
			     chosePlayer = true   

		 } 
		
		  world(fieldContainer(i).position.row) (fieldContainer(i).position.column).setHolder(tempFieldHolderID)
		  playerFieldCount(tempFieldHolderID) += 1	  
	  }
	  for(i <- 0 until avatarContainer.size)
	  {
	    avatarContainer(i).occupiedTerritory = playerFieldCount(avatarContainer(i).id) 
	  }
	}
  	
  	
  	/*
	 * Read fields from Maps-File and initialize the Position to the World.
	 * @ map: chosed map setup.
	 * @ return: all Fields in an Array.
	 */
	def initFieldInWorld(map:String):Array[Field] =
	{
		val fu = new FileUtil
		var file = mapDir + map
		var outArray = fu.readData(file)
		var Position = new Array[Int](2)
		var fieldContainer = new Array[Field](outArray.size)
		for(a <- 0 until outArray.size)
		{
		  var s = outArray.apply(a)
		  var charArray = s.toCharArray
		  // row 
		  Position(0) = String.valueOf(charArray(0)).toInt
		  // column

		  if(charArray.length == 3)	Position(1) = String.valueOf(charArray(2)).toInt
		  else if(charArray.length == 4)	Position(1) = String.valueOf(charArray(3)).toInt + 10
		  fieldContainer(a) = new Field(Position(0), Position(1)) 
		  world(Position(0)) (Position(1)) = fieldContainer(a)
		}
		fieldContainer
	}
  	
	
	/*
	 * Initialize number of players.
	 * @ humanPlayer: Number of human player.
	 * @ botPlayer: Number of KI player.
	 * @ return: Container with all players avatars.
	 * */
	def initPlayer(humanPlayer:Integer, botPlayer:Integer):Array[Avatar] =
	{
	  val avatarContainer = new Array[Avatar](humanPlayer + botPlayer)
	  val allPlayer = humanPlayer + botPlayer
	  for(i <- 0 until allPlayer)
	  {
	    if(i < humanPlayer)
	    {
		      avatarContainer(i) = new Avatar(i)
		      avatarContainer(i).color = Avatar.colorContainer(i)
	    }else
	    {
		    avatarContainer(i) = new Bot(i, this)
		    avatarContainer(i).color = Avatar.colorContainer(i)
	    }
	  }
	 this.avatarContainer = avatarContainer
	  avatarContainer
	}
	
	
	/*
	 * Initialize the world as a 18x10 game matrix with waterfields.
	 * */
	def initWorld
	{
	  for(i <- 0 until World.height; j <- 0 until World.width)
	  {
	    world(i)(j) = new Water(i,j)
	  }
	}
	
  	
	/*
	 * Send command to show the map selection menu
	 * */
  	def startShowMapSelectionMenu
	{
	  var notification = new Notification(Notification.MapSample)
	  notifyObservers(notification);
	}
  	
  	
  	/*
  	 * Game logic functionen
  	 * */
  	
  	
    /*
	 * Request an answer if the users wont to start the attack phase
	 */
	def attack
	{
	  sendNotificationMessage(Message.Info,"Angreifen? (ja/nein)")
	  currentInputType = "question"
	  currentPhase +=1
      sendAnswerRequest
	}
	
	
	/*
	 * Request an answer of the question
	 * if the user wont to continue attack an another land
	 * */
	def attackAnOtherLand
	{      
		if(checkPossibleToBattle(currentPlayer))
		{
			sendNotificationMessage(Message.Info,"Moechtest du ein anderes Land Angreifen? (ja/nein)")
			currentInputType = "question"
			currentPhase = 16
	   		sendAnswerRequest 
		}
		else
		{
			currentPhase = 17
			gameHandler
		}
	}
	
	
	/*
	 * Handling the answer of the question
	 * if the user wont to continue attack an another land
	 * */
	def attackAnOtherLandContinue
	{
	  	if(currentPlayer.answer)
		{
			currentPhase = 7
		}else
		{
			currentPhase = 17
		}
	  	resetFromAndToLand
  		gameHandler
	}
	
	
	/*
	 * Take actions according the answer of the user
	 * if he wont's to attack again
	 * */
	def attackContinue
	{
	    if(currentPlayer.answer)
			  {
				  currentPhase += 1
			  }else
			  {
			    resetFromAndToLand
			    notifyObservers(new Notification(Notification.DrawUI))
			    sendNotificationMessage(Message.Info, "Angriff wurde abgebrochen.")
			    currentPhase = 15
			  }
  		gameHandler
	}
	
	
	/*
	 * Handling the attack
	 * */
	def attackHandler
	{
		if(fromLand.getArmy > 1)
		{
		    singleAttack(fromLand, toLand)
		}
		else
			currentPhase = 13
	}
	
	
	/*
	 * Ask if the users wont to attack the same land again
	 * */
	def attackSameLand
	{
	  		if(fromLand.getArmy != 1)
			{
			   	sendNotificationPlayerMessage(currentPlayer, "Spieler " + currentPlayer.id)
			   	sendNotificationMessage(Message.Info, ": Noch verbliebene Einheiten: " + fromLand.getArmy)
			
			   	sendNotificationPlayerMessage(getAvatar(toLand.getHolder), "Spieler " + toLand.getHolder)
			    sendNotificationMessage(Message.Info, ": Noch verbliebene Einheiten: " + toLand.getArmy)
			    sendNotificationMessage(Message.Info,"Weiter angreifen? (ja/nein)")
			    currentInputType = "question"
			    currentPhase = 13
			    sendAnswerRequest
			}
	  		else 
		    {
	  		  sendNotificationPlayerMessage(currentPlayer,"Spieler " + currentPlayer.id)
	  		  sendNotificationMessage(Message.Info,": Angriff gescheitert")
	  		  currentPhase = 15
	  		  gameHandler
			}
	}
	
	
	/*
	 * Handling the user input if he whishes to continue 
	 * attacking the same land
	 * */
	def attackSameLandContinue
	{
	    if(currentPlayer.answer)
		{
			currentPhase = 11
		}else
		{
			currentPhase = 15
		}
  		gameHandler
	}
  	
  	
	/*
	 * Request the user to select the land for attacking
	 * */
  	def battlePhaseChooseEnemyLand
	{
	  	if(!gameOver)
		{			
  			notifyObservers(new Notification(Notification.DrawUI))
	  		sendNotificationMessage(Message.Info, "Waehle das Land, welches du angreifen willst.")    
	  		currentInputType = "position"
	  		currentRequestedPositionType = 2
	  		//sendInputRequest
		}
	}
  	
  	
  	/*
  	 * Request the user to select his own land for attacking
  	 * */
 	def battlePhaseChooseOwnLand
	{
	  	if(!gameOver && checkPossibleToBattle(currentPlayer))
		{			
  			notifyObservers(new Notification(Notification.DrawUI))
	  		sendNotificationMessage(Message.Info, "Waehle das Land, von welchem du aus angreifen willst.")    
	  		currentInputType = "position"
	  		currentRequestedPositionType = 1
	  		//sendInputRequest
		}else if(!checkPossibleToBattle(currentPlayer))
		{
		  currentPhase = 17
		  gameHandler
		}
	}
  	
 	
  	/*
  	 * Handling battle phase. 
  	 * */
  	def battlePhaseContinue
  	{
  		if(currentPlayer.answer)
		{
			currentPhase = 7
		}else
		{
			currentPhase = 17
		}
  		gameHandler
  	}
  	
  	
  	/*
	 *  Start the tactical phase.
	 *  The players have one time the right move the units
	 *  between his own territory,
	 *  Both territory must be neighbours and min. one unit
	 *  must be positioned. 
	 * */
	def startTactic
	{
	  resetFromAndToLand
		  if(checkPossibleForTactic)
		  {   
			  sendNotificationMessage(Message.Info,"Taktische Phase!\nMoechtest du Einheiten verschieben? (ja/nein)")
			  currentInputType = "question"
			  currentPhase = 18
			  sendAnswerRequest
		  }else
		  {
		    currentPhase = 22
		    gameHandler
		  }	 
	}

	
	/*
	 * Handler for tactical move action
	 * */
	def tacticMoveHandler
	{
		sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
		moveOption = 2
	   	currentInputType = "amount"
	   	sendUnitAmountRequest
	}
	
	
	/*
	 * Handling the tactical phase
	 * */
	def tacticPhaseContinue
	{
	  if(currentPlayer.answer)
		{
			currentPhase = 19
		}else
		{
			currentPhase = 22
		}
  		gameHandler
	}
	
	
	/*
	 * Request to the user to choose a first territory for a tactical move
	 * */
	def tacticPhaseChooseFirstLand
	{
	  sendNotificationUI
	  sendNotificationMessage(Message.Info,"Bitte waehle ein eigenes Land aus um Truppen zu verschieben")
	  
	  currentInputType = "position"
	  currentRequestedPositionType = 1
	  //sendInputRequest
	}
	
	
	/*
	 * Request to the user to choose a second territory for a tactical move
	 * */
	def tacticPhaseChooseSecondLand
	{
	  sendNotificationUI
	  sendNotificationMessage(Message.Info,"Bitte waehle ein weiteres eigenes Land aus um Truppen darauf zu verschieben")	  
	  currentInputType = "position"
	  currentRequestedPositionType = 3
	}
	
	
	/*
	 * Start the new round for the next player.
	 * A round includs reinforcement, battle and tactical phase
	 * */
	def newRound
	{
	  var indexOfNextPlayer = avatarContainer.indexOf(currentPlayer)+1
	  if(indexOfNextPlayer == avatarContainer.size)
	    indexOfNextPlayer = 0
	  
	  currentPlayer = avatarContainer(indexOfNextPlayer)
	  currentPhase = 2
	  resetFromAndToLand
	  
	  if(currentPlayer.lost)
		  newRound
	  else
	    gameHandler
	}
	
	
	
    /*
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @ player: Should be the player who will verify with correlated land.
	 * @ position: The position for that will check the neighbour land.
	 * @ return: true if the position has a neighbour which belongs to the player.
	 * */
	def checkHasOwnNeighbour(player:Avatar,position:WorldPosition):Boolean =
	{
	  var ownLand = world(position.row)(position.column)
	  
	  var neighbourContainer = new ArrayBuffer[WorldPosition]()
	  
	  neighbourContainer += new WorldPosition(position.row, position.column-1)
	  neighbourContainer += new WorldPosition(position.row, position.column+1)
	  neighbourContainer += new WorldPosition(position.row+1, position.column)
	  neighbourContainer += new WorldPosition(position.row-1, position.column)
	  
	  var hasOwnNeighbour = false
	  neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))

	  if(neighbourContainer.length != 0)
	  {
	    for(i <- 0 until neighbourContainer.length)
	    {
	      var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
	      if(neighbourLand.getHolder == ownLand.getHolder && neighbourLand.getFieldType)
	      {
	        hasOwnNeighbour = true
	      }
	    }
	  }else
	    hasOwnNeighbour = false     
	    
	    hasOwnNeighbour
	}
	
	
	/*
	 * Check if the player can do a tactical move
	 * @ return: true or false
	 * */
	def checkPossibleForTactic:Boolean =
	{
	  var isTacticPossible = false
	  var possibleTacticLandContainer = fieldContainer.filter(field => field.checkHolder(currentPlayer))
	  if(possibleTacticLandContainer.length != 0)
	  {
	    possibleTacticLandContainer =  possibleTacticLandContainer.filter(field=>checkTacticLandSelection(field.position,true))
	  }
	  
	  if(possibleTacticLandContainer.length != 0 )
	    isTacticPossible = true
	  else
	  {
	    isTacticPossible = false
	  }
	  isTacticPossible
	}
	
	
	/*
	 * Check if the selected land for the tactic process is valid.
	 * @ player: Should be the current player of the game.
	 * @ position: The position of the selected land.
     * @ isFrom: The function require the value true to set the (source-) from-land, otherwise the (target-) to-land.  
	 * @ return true or false
	 * */
	def checkTacticLandSelection(position:WorldPosition, isFrom:Boolean):Boolean =
	{
	  var ok = false
	  
	  if(isFrom)
	  {
		  if(checkOwnLandSelection(currentPlayer, position) && checkHasOwnNeighbour(currentPlayer, position) && checkEnoughArmy(position))
		   ok = true
		  else
		   ok = false
	  }else
	  {
	    if(checkOwnLandSelection(currentPlayer, position) && fromLand.checkNeighbourhood(world(position.row)(position.column)) )
	    	ok = true
	    else
	    	ok = false
	  }
	  ok
	}
  	
  	
	/*
	 * Handling the gameplay logic on the basis of the current phase
	 * */
  	def gameHandler
    {
    	  if(gameOver)
    	  {
    		 exitGame  
    	  }
    	  else if(currentPlayer.isInstanceOf[Bot])
    	  {
			 activateBotMove
    	  }
    	  else
    	  {
    	    currentPhase match
    	    {
    	      
    	      case 0 	=> configPlayer
    	      case 1	=> sendMapSample
    	      case 2 	=> startReinforcement
    	      case 3 	=> reinforcementPhase
    	      case 4 	=> handleReinforcement
    	      case 5 	=> startBattle
    	      case 6 	=> battlePhaseContinue
    	      case 7 	=> battlePhaseChooseOwnLand
    	      case 8 	=> battlePhaseChooseEnemyLand
    	      case 9 	=> attack
    	      case 10  	=> attackContinue
    	      case 11	=> attackHandler
    	      case 12	=> attackSameLand
    	      case 13	=> attackSameLandContinue
    	      case 14	=> moveHandler
    	      case 15	=> attackAnOtherLand
    	      case 16	=> attackAnOtherLandContinue
    	      case 17	=> startTactic
    	      case 18	=> tacticPhaseContinue
    	      case 19	=> tacticPhaseChooseFirstLand
    	      case 20	=> tacticPhaseChooseSecondLand
    	      case 21	=> tacticMoveHandler
    	      case 22	=> newRound  	      
    	    }
    	  }
    }
  	
  	
  	def activateBotMove 
  	{
  	  var bot:Bot =  currentPlayer.asInstanceOf[Bot]
		     bot.startReinforcementPhase
		     bot.startBattlePhase
			 if(!this.gameOver)
				 bot.startTacticPhase
			 else
			 {
			   exitGame
			 }
  	  newRound
  	}
  	
  	
  	/*
  	 * Command for the views to start with the player config menu
  	 * */
  	def configPlayer
  	{
  		currentInputType = "playerInit"
  		var n = new Notification(Notification.PlayerInit)  
  		notifyObservers(n)     
  	}
  	
  	
  	/*
  	 * Command for the views to show the map selection menu
  	 * */
  	def sendMapSample
  	{
  		currentInputType = "map"
  		var n = new Notification(Notification.MapSample)  
  		notifyObservers(n)
  	}
  	
  	
  	/*
	 * Handling reinforcement phase 
	 * */
	def handleReinforcement =
	{
		  fromLand.incArmy
		  currentPlayer.newUnitsTemporary -= 1
		  sendNotificationUI
		  if(currentPlayer.newUnitsTemporary == 0)
			  currentPhase = 5
		  else
			  currentPhase = 3
		  resetFromAndToLand
		  gameHandler
	}
	
	
	/*
	 * Dispatch between moving action in 
	 * the battle or tactical phase
	 * @ amount: amount of units for this action
	 * */
	def manageUnitMove(amount:Int)
	{
	  moveOption match 
	  {
	    case 1 => moveOptionOne(amount)
	    case 2 => moveOptionTwo(amount)
	  } 
	}
	
		
	/*
	 * Move units at the battle phase
	 * @ amount: amount of units for this action
	 * */
	def moveOptionOne(amount:Int)
	{
		if(checkNumberOfUnitMove(amount))
	    {
	    	setArmyForAttackAndDefenseLand(fromLand, toLand, amount)
	    	currentPhase += 1 
	    }
	  	else
	    	sendNotificationMessage(Message.Error, "Die ausgewaehlte Anzahl Einheiten war nicht korrekt")
	  	sendNotificationUI
	    gameHandler
	}
	
	
	/*
	 * Move units at the tactical phase
	 * @ amount: amount of units for this action
	 * */
	def moveOptionTwo(amount:Int)
	{
	  if(checkTacticLandSelection(fromLand.position, true) && checkTacticLandSelection(toLand.position, false) && ((fromLand.getArmy - amount) >= 1))
	  {
		  	setArmyForAttackAndDefenseLand(fromLand, toLand, amount)
	    	currentPhase += 1 
	  }
	  else
		  sendNotificationMessage(Message.Error, "Die ausgewaehlte Anzahl Einheiten war nicht korrekt")
	  sendNotificationUI
	  gameHandler
	}
	
	
	/*
	 * Handler for moving units actions
	 * */
	def moveHandler
	{
		sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
		   
		sendNotificationPlayerMessage(getAvatar(toLand.getHolder), "Spieler " + fromLand.getHolder)
	 	sendNotificationMessage(Message.Info,":  Noch verbliebene Einheiten: " + fromLand.getArmy)
	   	currentInputType = "amount"
	   	moveOption = 1
	   	sendUnitAmountRequest 
	}
    
	
	/*
	 * Move the units from attack-land to the defense land with an assigned amount of units.
	 * Inform the User about the status and rules of the move
	 * @ attack: Current player Land which is responsible for the attack.
	 * @ defense: Former Foreign Land which is now a new own land for the owner of the attack land.
	 * @ attackCountDices: Number of Units to move.
	 * */
	def battleMoveUnit(attack: Land, defense: Land, minimumMove:Int)
	{  		 
	  var protectLandUnit = 1
	    	 if(attack.getArmy - protectLandUnit > minimumMove)
	    	 {
		    	   sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		    	   sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
		    	   
		    	   sendNotificationPlayerMessage(getAvatar(defense.getHolder), "Spieler " + attack.getHolder)
	    	       sendNotificationMessage(Message.Info,":  Noch verbliebene Einheiten: " + attack.getArmy)
	    	       currentPlayer.minimumMove = minimumMove
	    	       moveOption = 1
	    	       currentInputType = "amount"
	    	       currentPhase = 14
	    	       sendUnitAmountRequest    	      
	    	 }
	    	 else
	    	 { 
		    	 setArmyForAttackAndDefenseLand(fromLand, toLand, minimumMove)
		    	 sendNotificationUI
		    	 currentPhase = 15
		    	 gameHandler
	    	 }
	}
	
	
  	/*
  	 * Inform the user of the current phase (reinforcement) and sends a request to the user to 
  	 * select a territory 
  	 * */
  	def reinforcementPhase =
 	{  	
  		sendNotificationPlayerMessage(currentPlayer,"Spieler " + currentPlayer.id +":")
		sendNotificationMessage(Message.Info," ist dran.")
		sendNotificationMessage(Message.Info,"Zahl der Verstaerkung: " + currentPlayer.newUnitsTemporary)
		sendNotificationMessage(Message.Info,"Bitte Teritorium eingeben (Spalte,Zeile).")
		currentInputType = "position"
		currentRequestedPositionType = 1
 	}
  	
  	
  	/*
	 * Execute a single Attack. In case the chose lands was invalid the user will get an appropriate message.
	 * @ attack: Current player Land which is responsible for the attack.
	 * @ defense: Foreign Land which try to defend.
	 * */
	def singleAttack(attack: Land, defense: Land ) =
	{
	  if(attack.checkNeighbourhood(defense))
	  {
	    if(attack.getFieldType && defense.getFieldType)
	    {
	    	 // one unit must be stay on the attackers field
	    	 var protectLandUnit = 1
	    	 
	    	 // defines how many times the players can roll
		     var attackCountDices = attack.getArmy - protectLandUnit
		     var defenseCountDices = defense.getArmy
		      
		     attackCountDices match
		     {
				  case 1 => attackCountDices = 1
				  case 2 => attackCountDices = 2
				  case _ => attackCountDices = 3
		     }
		      
		     defenseCountDices match
		     {
				  case 1 => defenseCountDices = 1
				  case _ => defenseCountDices = 2
		     }
		      
		     var attackDice = new Array[Int](attackCountDices)
		     var defenseDice = new Array[Int](defenseCountDices)
		     var dice = new Dice
		     
		     // roll the dice for attacker
		     for(i <- 0 until attackDice.length)
		     {
		       attackDice(i) = dice.roll
		       sendNotificationPlayerMessage(getAvatar(attack.getHolder), "Spieler " + attack.getHolder)
		       sendNotificationMessage(Message.Info,": hat eine " + attackDice(i) + " gewuerfelt. ")
		     }
		      
		     // roll the dice for the defender
		     for(i <- 0 until defenseDice.length)
		     {
		       defenseDice(i) = dice.roll
		       sendNotificationPlayerMessage(getAvatar(defense.getHolder), "Spieler " + defense.getHolder)
		       sendNotificationMessage(Message.Info,": hat eine " + defenseDice(i) + " gewuerfelt. ")
		     }
		     
		     // sorting the dice eyes in correct order for matching
		     Sorting.quickSort(attackDice)
		     Sorting.quickSort(defenseDice)
		     attackDice = inverseArray(attackDice)
		     defenseDice = inverseArray(defenseDice)
		     
		     // match winner and loser with the highest dice eyes 
		     if(attackDice(0) > defenseDice(0))
		       defense.decArmy
		     else
		       attack.decArmy
		     // match winner and loser with the second highest dice eyes 
		     if(defenseCountDices > 1  && attackCountDices >1)
		     {
		    	if(attackDice(1) > defenseDice(1))
		    		defense.decArmy
		    	else
		    		attack.decArmy
		     }
		    sendNotificationUI
		    //  enemy territory is conquered
		    if(defense.getArmy == 0)
		    {
		        sendNotificationMessage(Message.Success,"Sieg!!")
		        var lostPlayer =  defense.getHolder
		        setValueForWinnerAndLoser(attack, defense)
		        
		        if(checkPlayerOutOfGame(lostPlayer) && checkGameOver)
		        {
		        	gameOver = true
		        	gameHandler
		        }
		        else
		        {
		          // move Units to this field
		          battleMoveUnit(attack, defense, attackCountDices)
		        }
		    }
		    else
		    {
		      currentPhase += 1
		      gameHandler
		    }
	    }else
	    {
	      sendNotificationMessage(Message.Error,"Ist kein Feld sondern Wasser")
	    }
	  }  
	  else
	    sendNotificationMessage(Message.Error,"Ist kein Feld sondern Wasser")
	}
  	
  	
  	/*
	 * Start the Battle Phase for the assigned player.
	 * Check if the player can battle and ask if they wont to battle.
	 * */
	def startBattle
	{
		  resetFromAndToLand
		  if(checkPossibleToBattle(currentPlayer))
		  {   
			  sendNotificationMessage(Message.Info, "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)")
			  currentInputType = "question"
			  currentPhase = 6
			  sendAnswerRequest			  
		  }else
		  {
		    sendNotificationMessage(Message.Info, "Battle Phase wird ausgesetzt, da nicht genug Einheiten auf eigene Laender stationiert sind die an feindliche Nachbarn grenzen .")
		    currentPhase = 17
		    gameHandler
		  }
	}
  	
  	
  	/*
	 * Start with the reinforment phase.
	 * Set a chose amount of Unit to chose field and to an assigned player.
	 * */
	def startReinforcement =
	{
		sendNotificationMessage(Message.Info, "                    Verstaerkungsphase!")
    	sendNotificationUI
    	currentPlayer.newUnitsTemporary = currentPlayer.getTerritories/Avatar.divider
		if (currentPlayer.newUnitsTemporary < 3) currentPlayer.newUnitsTemporary = 3
		currentPhase = 3
		gameHandler
	}
	
	
	/*
	 * Handler for locking the selected field
	 * @ pos: choosen territory
	 * */
	def setLandSelection(pos:WorldPosition)
	{
	  	currentRequestedPositionType match
  		{
  		  case 1 => setOwnLand(pos)
  		  case 2 => setEnemyLand(pos)
  		  case 3 => setOwn2Land(pos)
  		}
	}
	
	
	/*
	 * Lock the selected own territory
	 * @ pos: choosen territory
	 * */
	def setOwnLand(pos:WorldPosition)
	{
		val choosenLand = world(pos.row)(pos.column)
		if(fromLand == null)
		{
		  if(choosenLand.checkHolder(currentPlayer))
		  {
		    if(currentPhase != 7 && currentPhase != 19 || ((currentPhase == 7 || currentPhase ==19) && checkEnoughArmy(pos)) )
		    {   
		    	fromLand = choosenLand
			    currentPhase += 1
			    sendNotificationUI
		    }else if(!checkEnoughArmy(pos))
		      sendNotificationMessage(Message.Error,"Auf dem ausgewaehltem Land befinden sich zu wenig Einheiten.")
		    
		  }else
		    sendNotificationMessage(Message.Error,"Das Teritorium ist nicht dein Land.")   
		}
		else 
		  println("Debug: ERROR beim setzen vom fromland")
		gameHandler
	}
	
	
	/*
	 * Lock the selected territory for attacking
	 * @ pos: choosen territory 
	 * */
	def setEnemyLand(pos:WorldPosition)
	{
	  var choosenLand = world(pos.row)(pos.column)
	  if(fromLand == choosenLand)
	  {
		  sendNotificationMessage(Message.Info, "Bereits ausgewaehltes Land wurde deselectiert!")
		  resetFromAndToLand
		  currentPhase -= 1
		  sendNotificationUI
	  }
	  else if(checkEnemyLandSelection(currentPlayer, pos))
	  {
	      toLand = choosenLand
	      currentPhase += 1;
	      sendNotificationUI
	  }
	  else
		  sendNotificationMessage(Message.Error,"Ausgewaehltes Land ist dein eigenes.")
	  gameHandler
	}
	
	
	/*
	 * Select and lock the choosen second territory for tactical move actions
	 * @ pos: choosen territory
	 * */
	def setOwn2Land(pos:WorldPosition)
	{
		val choosenLand = world(pos.row)(pos.column)
		if(fromLand == choosenLand)
		{
		  sendNotificationMessage(Message.Info, "Bereits ausgewaehltes Land wurde deselectiert!")
		  resetFromAndToLand
		  currentPhase -= 1
		  sendNotificationUI
	  	}
		else if(toLand == null)
		{
		  if(choosenLand.checkHolder(currentPlayer) && fromLand.checkNeighbourhood(choosenLand))
		  {
		    toLand = choosenLand
		    currentPhase += 1
		    sendNotificationUI
		  }else if(!fromLand.checkNeighbourhood(choosenLand))
		  {
		     sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland.")
		  }
		  else if(!choosenLand.checkHolder(currentPlayer))
		  {
		    sendNotificationMessage(Message.Error,"Das Teritorium ist nicht dein Land.")
		  }
		}
		else 
		{
		  println("Debug: ERROR beim setzen vom toLand")
		}
		gameHandler
	}
	
	
	/*
	 * Send request to user, they need to response a question
	 * */
	def questionResponse(response:Boolean) = 
	{
	  if(currentPhase == 0 )
	  {
	    sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	  }
	  currentPlayer.answer = response
	  gameHandler
	}
	
	
	/*
  	 * help functions
  	 * */
	
	
	/*
	 * Invert an Array in downward order.
	 * @ array: the given array to invert.
	 * @ return: the array in the inverse order
	 * */
	def inverseArray(array:Array[Int]):Array[Int]=
	{
	  var tempArray = new Array[Int](array.length);
	  var j = 0
	  for(i <- array.length -1 to 0 by -1)
	  {
	    tempArray(j) = array(i)
	    j = j + 1
	    
	  }
	  tempArray
	}
	
	
	
	/*
	 * Check is land in the world a water type, return true when the land type is water.
	 * @ position: Position in the world
	 * @ return: true or false
	 * */
	def isWaterLand(position:WorldPosition):Boolean =
	{
	  if(!world(position.row)(position.column).getFieldType)
		  true
	  else
	      false
	}
	
	
	/*
	 * Check for game over conditions
	 * @ return: true or false
	 * */
	def checkGameOver:Boolean =
	{
	  var numberOfActivePlayer = avatarContainer.filterNot(player=> player.lost).length
	  if(numberOfActivePlayer == 1)
	    true 
	  else
	      false
	}
	
	
	/*
	 * Check whether the amount of unit to move for the assigned land is allowed.
	 * When the amount is too high, the user will be informed about it.
	 * @ land: The army of this land be checked. 
	 * @ army: Number of desired units.
	 * @ minArmy: Minimum Units that have to be move.
	 * @ return: when the assigned values is allowed the function will return true otherwise false.
	 * */
	def checkNumberOfUnitMove(army:Int):Boolean =
	{
		if(fromLand.getArmy - army < 1)
		{
			sendNotificationMessage(Message.Error,"Zuviele Einheiten gewaehlt. Bitte erneut eingeben.")
			return false
		}else if(army < currentPlayer.minimumMove)
		{
		  sendNotificationMessage(Message.Error,"Es muessen mindestens " + currentPlayer.minimumMove + "  Einheiten verschoben werden, da mit sovielen Einheiten auch angegriffen wurde. Bitte erneut eingeben.")
		  return false
		}else if(army == 0)
		{
		  sendNotificationMessage(Message.Error,"Zu wenige Einheiten die verschoben werden sollen. Bitte erneut eingeben.")
		  return false
		}
	    else
	    	return true
	    false
	}
	
	
	/*
	 * Check whether the player with the given id has lost.
	 * @ id: Id of the player.
	 * @ return: the function return true when the player has lost.
	 * */
	def checkPlayerOutOfGame(id:Int):Boolean =
	{
	  var player = getAvatar(id)
	  if(player.occupiedTerritory == 0)
	  {
	    player.lost = true
	    true
	  }
	    else
	      false
	}

	
	/*
	 * Check if the constrains for selected territory are fullfilled
	 * @ pos: choosen territory
	 * @ return: true or false
	 * */
	def checkPositionConstrains(pos:WorldPosition):Boolean =
	{
		if(!checkPositionIsInWorld(pos))
		{
		  sendNotificationMessage(Message.Error,"Das Ausgewaehle Land befindet sich nicht auf dieser Welt.")
		  false
		}
		else if(isWaterLand(pos))
		{
		  sendNotificationMessage(Message.Error,"Ein Wasserfeld hat kein besitzer")
		  false
		}
		else
		  true
	}
	
	
	/*
	 * Check if the choosen field is within the world of this game
	 * @ position: choosen territory
	 * @ return: true or false
	 * */
	def checkPositionIsInWorld(position:WorldPosition):Boolean =
	{
	  if(position.row < 0 || position.row > World.height-1 || position.column < 0 || position.column > World.width -1)
	    false
	  else
	    true
	}
	
	
	/*
	 * Check if the player achieve the conditions to Battle
	 * @ player: player to validate.
	 * @ return: Is it possible to battle the function return true. 
	 */
	def checkPossibleToBattle(player:Avatar):Boolean =
	{
	  var isBattlePossible = false
	  var possibleBattleLandContainer = fieldContainer.filter(field => field.checkHolder(player))
	  if(possibleBattleLandContainer.length != 0)
	  {
	    possibleBattleLandContainer =  possibleBattleLandContainer.filter(field=>checkBattleLandSelection(player,field.position,true))
	  }
	  
	  if(possibleBattleLandContainer.length != 0 )
	    isBattlePossible = true
	  else
	  {
	    isBattlePossible = false
	  }
	  isBattlePossible
	}

	
	/*
	 * Check if this input is from right type or allowed at current state
	 * @ return: true or false
	 * */
	def checkInputTypeValidation(inputType:String): Boolean =
    {
      if(inputType == currentInputType)
        true
      else
        false
    }
	
	
	/*
	 * Check if the selected land for the battle process is valid.
	 * @ player: Should be the current player of the game.
	 * @ position: The position of the selected land.
	 * @ isOwnLand: Verify the own or enemy land. Is it true the function will verify the own land.
	 * @ return: true or false
	 * */
	def checkBattleLandSelection(player:Avatar, position:WorldPosition, isOwnLand:Boolean):Boolean =
	{
	  var ok = false
	  if(isOwnLand)
	  {
		  if(checkOwnLandSelection(player, position) && checkHasEnemyNeighbour(player, position) && checkEnoughArmy(position))
		   ok = true
		  else
		   ok = false
	  }else
	  {
	    if(checkEnemyLandSelection(player, position))
	    	ok = true
	    else
	    	ok = false
	  }
	  ok
	}
	
	
	/*
	 * Validate whether the current player is owner of the land.
	 * @ player: Should be the current player.
	 * @ position: World position of the land.
	 * @ return: true when the selected land is from the current player.
	 * */
	def checkOwnLandSelection(player:Avatar,position:WorldPosition):Boolean =
	{
		var ownLand = world(position.row)(position.column)
		var ok = false
		if(ownLand.checkHolder(currentPlayer))
			ok = true
		else
			sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht ein eigenes Teritorium.")
		ok
	}
	
	
	/*
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @ player: Should be the player who will verify with correlated land.
	 * @ position: The position for that will check the neighbour land.
	 * @ return: true if the position has a neighbour which belongs another player.
	 * */
	def checkHasEnemyNeighbour(player:Avatar,position:WorldPosition):Boolean =
	{
	  var ownLand = world(position.row)(position.column)
	  
	  var neighbourContainer = new ArrayBuffer[WorldPosition]()
	  
	  neighbourContainer += new WorldPosition(position.row, position.column-1)
	  neighbourContainer += new WorldPosition(position.row, position.column+1)
	  neighbourContainer += new WorldPosition(position.row+1, position.column)
	  neighbourContainer += new WorldPosition(position.row-1, position.column)
	  
	  var hasEnemyNeighbour = false

	  neighbourContainer = neighbourContainer.filter(position => checkPositionIsInWorld(position))
	  
	  if(neighbourContainer.length != 0)
	  {
	    for(i <- 0 until neighbourContainer.length)
	    {
	      var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
	      if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType)
	      {
	        hasEnemyNeighbour = true
	      }
	    }
	  }else
	    hasEnemyNeighbour = false     
	    
	    hasEnemyNeighbour
	}
	
	
	/*
	 * Check whether the land has enough army to attack or to move.
	 * @ position: Position of the world.
	 * */
	def checkEnoughArmy(position:WorldPosition):Boolean =
	{
	  var ok:Boolean = false
	  if(world(position.row)(position.column).getArmy > 1)
	      ok = true
	  else
		  ok = false
	  ok
	}
	
	
	/*
	 * Validate whether the given position is a enemy neighbour of the current player.
	 * @ player: Should be the current player.
	 * @ position: World position of the land.
	 * @ return: true when the selected land is not from the current player.
	 * */
	def checkEnemyLandSelection(player:Avatar, position:WorldPosition):Boolean =
	{
		var otherLand = world(position.row)(position.column)
		var ownLand = fromLand
		var ok = false
		if(ownLand.checkNeighbourhood(otherLand) && !otherLand.checkHolder(player) && !isWaterLand(position))
			ok = true
		else if (!ownLand.checkNeighbourhood(otherLand))
			sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland.")
		else if (otherLand.checkHolder(player))
			sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist ein eigenes Teritorium.")
		else if (isWaterLand(position))
			sendNotificationMessage(Message.Error,"Ein Wasserfeld hat kein besitzer")
		ok		 
	}
	
	
	/*
	 * Search for the avatar with this specific id and return it
	 * @ id: Identification of the avatar
	 * @ return: Avatar if found else null
	 * */
	def getAvatar(id:Int):Avatar =
	{
	  val player = avatarContainer.find(avatar => avatar.id == id)
	  var avatar:Avatar = null
	  player match
	  {
	     case Some(foundAvatar) => avatar = foundAvatar
	     case None => println("Debug Game.getAvatar: Kein Avatar gefunden")
	  }
	  avatar
	}
	
	
	/*
	 * Reset the attribute fromLand and toLand
	 * */
	def resetFromAndToLand
	{
	  fromLand = null
	  toLand = null
	}
	
	
	/*
	 * communication functions
	 * */

	/*
	 * Send a request message to the user, they need to 
	 * confirm a yes/no-question
	 * */
	def sendAnswerRequest
	{
	  val n = new Notification(Notification.Question)
	  notifyObservers(n)
	}
	
	
	/*
	 * Send the command to close this application and to
	 * terminate all threads
	 * */
	def sendExit
	{
	  val n = new Notification(Notification.Exit)
	  notifyObservers(n)
	}
	
	
	/*
	 * Send a request message to the user, they need to 
	 * type in the amount of units for moving actions
	 * */
	def sendUnitAmountRequest
	{
	  val n = new Notification(Notification.Move)
	  notifyObservers(n)
	}
	
	
	/*
	 * Send a notification to all observer that the game is over
	 * @ winner: defines the player who won the game
	 * */
	def sendNotificationGameOver(winner:Avatar)
	{
	  sendNotificationMessage(Message.Info, "Spiel Ende")
	  sendNotificationPlayerMessage(getAvatar(winner.id), "Spieler " + winner.id)
	  sendNotificationMessage(Message.Success, " hat Gewonnen !!!")
	  var nGameOver = new Notification(Notification.GameOver)
	  nGameOver.currentPlayer = winner
	  notifyObservers(nGameOver)
	}
	
	
	/*
	 * Send notifications to Inform the User.
	 * @ messageType: Responsible for the color of the output.
	 * @ messageContent: Informations for the user.
	 * */
	def sendNotificationMessage(messageType:Message.MessageTyp, messageContent:String)
	{
	  var notificationMessage = new Notification(Notification.Message)
	  var message = new Message(messageType, messageContent)
	  notificationMessage.message = message
	  wait(() => 
	  {
		notifyObservers(notificationMessage)
	  }, 200)
	  
	}
	
	def wait(callback: () => Unit, milliSeconds:Long) {
		callback()
		Thread sleep milliSeconds	
	}
	
	/*
	 * Send notifications to Inform the User.
	 * @ player: Responsible for the color of the output.
	 * @ messageContent: Informations for the user.
	 * */
	def sendNotificationPlayerMessage(player:Avatar, messageContent:String)
	{
	  var notificationMessage = new Notification(Notification.Message)
	  var message = new Message(Message.Player, messageContent)
	  notificationMessage.message = message
	  notificationMessage.currentPlayer = player 
	  wait(() => 
	  {
		notifyObservers(notificationMessage)
	  }, 200)
	}

	
	/*
	 * Send Notification to every UI to redraw the UI.
	 * */
	def sendNotificationUI()
	{
	  var notification = new Notification(Notification.DrawUI)
	  notifyObservers(notification);
	}
	
	
	/*
	 * Decrease and Increase the occupiedTerritory attribute for the players who hold this land.
	 * @decLand: Decrease the occupiedTerritory of the player about the amount 1
	 * @incLand: Increase the occupiedTerritory of the player about the amount 1
	 * */
	def setNewOccupiedTerritory(incLand:Land, decLand:Land )
	{
	  getAvatar(decLand.holder).occupiedTerritory -= 1
	  getAvatar(incLand.holder).occupiedTerritory += 1
	}
	
	
	/*
	 * Informs the user about the player setup
	 * @ playerCount: amount of human players
	 * @ botCount: amount of KI players
	 * @ return: if done correctly return true else false
	 * */
	def sendPlayerConfigMessage(playerCount:Int, botCount:Int):Boolean = 
	{
	  var correct = false
	  if((playerCount + botCount) > 3)
    	    sendNotificationMessage(Message.Error,"Es koennen insgesamt hoechsten drei mitspielen.")
    	  else if(playerCount < 1)
    	     sendNotificationMessage(Message.Error,"Es wird mindestens ein Spieler benoetigt. ")
    	  else
    	    correct = true
    	 
      correct
	}
	
	
	/*
	 * Increase and decrease the units about the assigned amount for the attack- and defense-land.
	 * @ attack: The territory form which the units will be taken from
	 * @ otherLand: The territory to which the units will be moved
	 * @ army: Number of units to set
	 * */
	def setArmyForAttackAndDefenseLand(attack:Land, otherLand:Land, army:Int) 
	{
		 attack.setArmy(attack.getArmy - army)
		 otherLand.setArmy(otherLand.getArmy + army)
	}
	
	
	/*
	 * Set the new holder which is the attack land for the beaten land.
	 * @ winLand: Territory which invades
	 * @ lostLand: Territory which was conquered
	 * */
	def setNewHolderForBeatenLand(winLand:Land, lostLand:Land) = lostLand.holder = winLand.holder
	
	
	/*
	 * Set all requiered values after a successfully singleattack for  the loser and winner.
	 * @ winLand: Current player Land which is responsible for the attack.
	 * @ lostLand: Enemy Land which is now a new own land for the owner of the attack land.
	 * */
	def setValueForWinnerAndLoser(winLand: Land, lostLand: Land)
	{
		    	setNewOccupiedTerritory(winLand, lostLand)
		        setNewHolderForBeatenLand(winLand, lostLand)
	}
}