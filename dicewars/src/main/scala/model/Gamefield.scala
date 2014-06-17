package main.scala.model

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.collection.immutable.StringOps
import main.scala.util.FileUtil
import scala.util.Random
import main.scala.util._
import main.scala.util.Message

class Gamefield extends Observable
{
  	var avatarContainer:Array[Avatar] = null //initPlayer(2)
  	var fieldContainer:Array[Field] = null
	
	var fromLand:Land = null
	var toLand:Land = null
	var gameOver:Boolean = false
	//phase meaning 0=> Init., 1 => Reinforcement, 2=>Battle, 3=>Tactic
	var currentPhase:Int = 0
	
	var moveOption:Int = 1
	
	// path to Maps
	val mapDir:String = "Maps/"
	
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](World.height,World.width)
	
	var currentPlayer:Avatar = null
	private var	currentInputType:String = ""
	private var currentRequestedPositionType: Int = 0
  	
	initWorld
	
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
	
  	
  	def exitGame
	{
	  sys.exit
	}
  	
	
  	/**
	 * Initialize all Fields and they appended holder.
	 */
	def initGame(map:String)
	{
	  fieldContainer = initFieldInWorld(map)
	  initFieldHolder(fieldContainer)
	}
  	
  	
  	/**
	 * Assigns player to created Fields.
	 * @param require an Array with all Fields
	 */
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
		 println("anzahl felder: " + fieldCount/playerCount)
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
  	
  	
  	/**
	 * Read fields from Maps-File and initialize the Position to the World.
	 * @param chosed map file.
	 * @return all Fields in an Array.
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
  	
	
	/**
	 * Initialize number of players.
	 * @param numberofPlayer. Number of player.
	 */
	def initPlayer(allPlayer:Integer, botPlayer:Integer):Array[Avatar] =
	{
	  val avatarContainer = new Array[Avatar](allPlayer)
	  val humanPlayer = allPlayer - botPlayer
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
	
	
	/**
	 * Initialize the world as a 18x10 game matrix with waterfields.
	 * */
	def initWorld
	{
	  for(i <- 0 until World.height; j <- 0 until World.width)
	  {
	    world(i)(j) = new Water(i,j)
	  }
	}
	
  	
  	def startShowMapSelctionMenu
	{
	  var notification = new Notification(Notification.MapSample)
	  notifyObservers(notification);
	}
  	
  	
  	/*
  	 * Game logic functionen
  	 * */
  	
  	
  	 		/**
	 * Manage the count of the attacks and communicate with the user about it.
	 * @param player. Should be the current player of the game.
	 */
	def attack
	{
	  var outloop = false
	  
	  sendNotificationMessage(Message.Info,"Angreifen? (ja/nein)")
	  currentInputType = "question"
	  currentPhase +=1
      sendAnswerRequest
	}
	
	
	def attackAOtherLand
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
	
	def attackAOtherLandContinue
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
	
	
	def attackHandler
	{
		if(fromLand.getArmy > 1)
		{
		    singleAttack(fromLand, toLand)
		}
		else
			currentPhase = 13
	}
	
	
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
  	
  	
 	def battlePhaseChooseOwnLand
	{
	  	if(!gameOver)
		{			
  			notifyObservers(new Notification(Notification.DrawUI))
	  		sendNotificationMessage(Message.Info, "Waehle das Land, von welchem du aus angreifen willst.")    
	  		currentInputType = "position"
	  		currentRequestedPositionType = 1
	  		//sendInputRequest
		}
	}
  	
  	
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
	 * Taktische Phase der Spieler hat einmal das Recht
	 * seine Einheiten von max. einem Land zu einem anderen Land,
	 * das ihn gehoert, zu verschieben.
	 * Danach ist die Runde fuer den Spieler Beendet und der 
	 * Naechste ist an der Reihe.
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
	  
	
	      
	      
//		  if(player.answer)
//		  {	
//			  sendNotificationUI
//			    do
//			    {
//			      sendTacticNotification(player,true)
//			    }while(!player.inputCorrect)
//			      
//			    do
//			    {
//			      sendTacticNotification(player,false)
//			    }while(!player.inputCorrect)
//			    
//			    setArmyToMove(fromLand, toLand, player)
//			    sendNotificationUI
//			    resetFromAndToLand
//
//		  }
//	  }else
//	  {
//	    sendNotificationMessage(Message.Info, "Taktische Phase wird ausgesetzt, da kein Land mit ausreichend Einheiten an ein eigenes Nachbarland angrenzt .")
//	  }
		 
	}

	
	def tacticMoveHandler
	{
		sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
		moveOption = 2
	   	currentInputType = "amount"
	   	sendUnitAmountRequest
	}
	
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
	
	def tacticPhaseChooseFirstLand
	{
	  sendNotificationUI
	  sendNotificationMessage(Message.Info,"Bitte waehle ein eigenes Land aus um Truppen zu verschieben")
	  
	  currentInputType = "position"
	  currentRequestedPositionType = 1
	  //sendInputRequest
	}
	
	def tacticPhaseChooseSecondLand
	{
	  sendNotificationUI
	  sendNotificationMessage(Message.Info,"Bitte waehle ein weiteres eigenes Land aus um Truppen darauf zu verschieben")	  
	  currentInputType = "position"
	  currentRequestedPositionType = 3
	  //sendInputRequest
	}
	
	def newRound
	{
	  var indexOfNextPlayer = avatarContainer.indexOf(currentPlayer)+1
	  if(indexOfNextPlayer == avatarContainer.size)
	    indexOfNextPlayer = 0
	  
	  currentPlayer = avatarContainer(indexOfNextPlayer)
	  currentPhase = 2
	  
	  if(currentPlayer.lost)
		  newRound
	  else
		gameHandler
	}
	
	
	
		/**
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @param player. Should be the player who will verify with correlated land.
	 * @param position. The position for that will check the neighbour land.
	 * @return return true if the position has a neighbour which belongs to the player.
	 */
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
		
			/**
	 * Check if the selected land for the tactic process is valid.
	 * @param player. Should be the current player of the game.
	 * @param position. The position of the selected land.
     * @param isFrom. The function require the value true to set the (source-) from-land, otherwise the (target-) to-land.  
	 */
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
    	  {// noch eine Phase hinzuzufuegen nach Phase 0 fuer das Mapsample
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
    	      case 15	=> attackAOtherLand
    	      case 16	=> attackAOtherLandContinue
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
  	
  	
  	def configPlayer
  	{
  	  currentInputType = "playerInit"
  	  var n = new Notification(Notification.PlayerInit)  
      notifyObservers(n)     
  	}
  	
  	def sendMapSample
  	{
  	  //sendNotificationMessage(Message.Info, "Bitte Map auswaehlen")
  	  currentInputType = "map"
  	  var n = new Notification(Notification.MapSample)  
      notifyObservers(n)
      
  	}
  	
  	
  	/**
	 * Check whether the reinforcement is correct, otherwise the user get an appropriate message.
	 * @param player. Current player for the reinforcement.
	 * @param position. Land Position for the Reinforcement.
	 */
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
	
	
	def manageUnitMove(amount:Int)
	{
	  moveOption match 
	  {
	    case 1 => moveOptionOne(amount)
	    case 2 => moveOptionTwo(amount)
	  } 
	}
	
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
    
	
	/**
	 * Move the units from attack-land to the defense land with an assigned amount of units.
	 * Inform the User about the status and rules of the move
	 * @param attack. Current player Land which is responsible for the attack.
	 * @param defense. Former Foreign Land which is now a new own land for the owner of the attack land.
	 * @param attackCountDices. Number of Units to move.
	 */
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
	    	 else{ 
		    	 setArmyForAttackAndDefenseLand(fromLand, toLand, minimumMove)
		    	 sendNotificationUI
		    	 currentPhase = 15
		    	 gameHandler
	    	 }
	}
	
  	
  	def reinforcementPhase =
 	{  	
		sendNotificationPlayerMessage(currentPlayer,"Spieler: " + currentPlayer.id)
		sendNotificationMessage(Message.Info," ist dran.")
		sendNotificationMessage(Message.Info,"Zahl der Verstaerkung: " + currentPlayer.newUnitsTemporary)
		sendNotificationMessage(Message.Info,"Bitte Teritorium eingeben (Spalte,Zeile).")
		currentInputType = "position"
		currentRequestedPositionType = 1
		//sendInputRequest
 	}
  	
  	
  	/**
	 * Execute a single Attack. In case the chose lands was invalid the user will get an appropriate message.
	 * @param attack. Current player Land which is responsible for the attack.
	 * @param defense. Foreign Land which try to defend.
	 */
	def singleAttack(attack: Land, defense: Land ):Unit ={
	  if(attack.checkNeighbourhood(defense))
	  {
	    if(attack.getFieldType && defense.getFieldType)
	    {
	    	  var protectLandUnit = 1
	      
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
		     for(i <- 0 until attackDice.length)
		     {
		       attackDice(i) = dice.roll
		       sendNotificationPlayerMessage(getAvatar(attack.getHolder), "Spieler " + attack.getHolder)
		       sendNotificationMessage(Message.Info,": hat eine " + attackDice(i) + " gewuerfelt. ")
	//	       println("Wuerfel Augen: " + attackDice(i))
		     }
		     for(i <- 0 until defenseDice.length)
		     {
		       defenseDice(i) = dice.roll
		       sendNotificationPlayerMessage(getAvatar(defense.getHolder), "Spieler " + defense.getHolder)
		       sendNotificationMessage(Message.Info,": hat eine " + defenseDice(i) + " gewuerfelt. ")
		     }
		     Sorting.quickSort(attackDice)
		     Sorting.quickSort(defenseDice)
		     attackDice = inverseArray(attackDice)
		     defenseDice = inverseArray(defenseDice)
		     
		     // Fuer ersten Wuerfel
		     if(attackDice(0) > defenseDice(0))
		       defense.decArmy
		     else
		       attack.decArmy
		     // Fuer zweiten Wuerfel
		     if(defenseCountDices > 1  && attackCountDices >1)
		     {
		       println("Debug:Defense: " + defenseDice.length)
		       println("Debug:Attack: " + attackDice.length)
		       println("Debug:defenseCountDices "+ defenseCountDices)
		       println("Debug:attackCountDices "+ attackCountDices)
		    	if(attackDice(1) > defenseDice(1))
		    		defense.decArmy
		    	else
		    		attack.decArmy
		     }
		    sendNotificationUI
	     
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
  	
  	
  	/**
	 * Start the Battle Phase for the assigned player.
	 * @param player. Should be the current player.
	 */
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
		    // Taktische phase
		    currentPhase = 17
		    gameHandler
		  }
	}
  	
  	
  	/**
	 * Set a chose amount of Unit to chose field and to an assigned player.
	 * @param player who makes a reinforcement.
	 */
	def startReinforcement =
	{
		sendNotificationMessage(Message.Info, "                    Verstaerkungsphase!")
    	sendNotificationUI
    	currentPlayer.newUnitsTemporary = currentPlayer.getTerritories/Avatar.divider
		if (currentPlayer.newUnitsTemporary < 3) currentPlayer.newUnitsTemporary = 3
		currentPhase = 3
		gameHandler
	}
	
	
	def setLandSelection(pos:WorldPosition)
	{
	  	currentRequestedPositionType match
  		{
  		  case 1 => setOwnLand(pos)
  		  case 2 => setEnemyLand(pos)
  		  case 3 => setOwn2Land(pos)
  		}
	}
	
	
	def setOwnLand(pos:WorldPosition)
	{
		val choosenLand = world(pos.row)(pos.column)
		if(fromLand == null)
		{
		  if(choosenLand.checkHolder(currentPlayer))
		  {
		    fromLand = choosenLand
		    currentPhase += 1
		    sendNotificationUI
		  }
		  else
		    sendNotificationMessage(Message.Error,"Das Teritorium ist nicht dein Land.")
		}
		else 
		{
		  println("Debug: ERROR beim setzen vom fromland")
		}
		gameHandler
	}
	
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
	
	
	def questionResponse(response:Boolean) = 
	{
	  if(currentPhase == 0 )
	  {
	    sendNotificationMessage(Message.Error,"Diese Eingabeoption ist aktuell nicht moeglich. Bitte erneut etwas Eingeben!")
	    //sendInputRequest
	  }
	  currentPlayer.answer = response
	  gameHandler
	}
	
	
	  	/*
  	 * help functions
  	 * */
	
	
	/**
	 * Invert an Array in downward order.
	 * @param array. the given array to invert.
	 */
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
	
	
	
	/**
	 * Check is land in the world a water type, return true when the land type is water.
	 * @param position. Position in the world
	 */
	def isWaterLand(position:WorldPosition):Boolean =
	{
	  if(!world(position.row)(position.column).getFieldType)
		  true
	  else
	      false
	}
	
	def checkGameOver:Boolean =
	{
	  var numberOfActivePlayer = avatarContainer.filterNot(player=> player.lost).length
	  if(numberOfActivePlayer == 1)
	    true 
	  else
	      false
	}
	
	
	/**
	 * Check whether the amount of unit to move for the assigned land is allowed.
	 * When the amount is too high, the user will be informed about it.
	 * @param land. The army of this land be checked. 
	 * @param army. Number of desired units.
	 * @param minArmy. Minimum Units that have to be move.
	 * @return when the assigned values is allowed the function will return true otherwise false.
	 */
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
	
	
	/**
	 * Check whether the player with the given id has lost.
	 * @param id. Id of the player.
	 * @return the function return true when the player has lost.
	 */
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
	
	
	def checkPositionIsInWorld(position:WorldPosition):Boolean =
	{
	  if(position.row < 0 || position.row > World.height-1 || position.column < 0 || position.column > World.width -1)
	    false
	  else
	    true
	}
	
			/**
	 * Check if the player achieve the conditions to Battle
	 * @param player. player to validate.
	 * @return Is it possible to battle the function return true. 
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

	
	def checkInputTypeValidation(inputType:String): Boolean =
    {
      if(inputType == currentInputType)
        true
      else
        false
    }
	
	
	/**
	 * Check if the selected land for the battle process is valid.
	 * @param player. Should be the current player of the game.
	 * @param position. The position of the selected land.
	 * @param isOwnLand. Verify the own or enemy land. Is it true the function will verify the own land.
	 */
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
	
	
		/**
	 * Validate whether the current player is owner of the land.
	 * @param player. Should be the current player.
	 * @param position. World position of the land.
	 * @return true when the selected land is from the current player.
	 */
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
	
	
		/**
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @param player. Should be the player who will verify with correlated land.
	 * @param position. The position for that will check the neighbour land.
	 * @return return true if the position has a neighbour which belongs another player.
	 */
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
	
	
	/**
	 * Check whether the land has enough army to attack or to move.
	 * @param position. Position of the world.
	 */
	def checkEnoughArmy(position:WorldPosition):Boolean =
	{
	  var ok:Boolean = false
	  if(world(position.row)(position.column).getArmy >=1)
	      ok = true
	  else
		  sendNotificationMessage(Message.Error,"Auf dem ausgewaehltem Land befinden sich zu wenig Einheiten.")
	  ok
	}
	
	
	/**
	 * Validate whether the given position is a enemy neighbour of the current player.
	 * @param player. Should be the current player.
	 * @param position. World position of the land.
	 * @return true when the selected land is not from the current player.
	 */
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
	
	/**
	 * Reset the attribute fromLand and toLand.
	 */
	def resetFromAndToLand
	{
	  fromLand = null
	  toLand = null
	}
	
	
	/*
	 * communication functions
	 * */
	
//	def sendInputRequest
//	{
//	  val n = new Notification(Notification.Input)
//	  notifyObservers(n)
//	}
//	
	
	def sendAnswerRequest
	{
	  val n = new Notification(Notification.Question)
	  notifyObservers(n)
	}
	
	
	def sendExit
	{
	  val n = new Notification(Notification.Exit)
	  notifyObservers(n)
	}
	
	
	def sendUnitAmountRequest
	{
	  val n = new Notification(Notification.Move)
	  notifyObservers(n)
	}
	
	
	def sendNotificationGameOver(winner:Avatar)
	{
	  sendNotificationMessage(Message.Info, "Spiel Ende")
	  sendNotificationPlayerMessage(getAvatar(winner.id), "Spieler " + winner.id)
	  sendNotificationMessage(Message.Success, " hat Gewonnen !!!")
	  var nGameOver = new Notification(Notification.GameOver)
	  nGameOver.currentPlayer = winner
	  notifyObservers(nGameOver)
	}
	
	
	/**
	 * Send notifications to Inform the User.
	 * @param messageType. Responsible for the color of the output.
	 * @param messageContent. Informations for the user.
	 */
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
	
	/**
	 * Send notifications to Inform the User.
	 * @param player. Responsible for the color of the output.
	 * @param messageContent. Informations for the user.
	 */
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

	
	/**
	 * Send Notification to every UI to redraw the UI.
	 */
	def sendNotificationUI()
	{
	  var notification = new Notification(Notification.DrawUI)
	  notifyObservers(notification);
	}
	
	
		/**
	 * Decrease and Increase the occupiedTerritory attribute for the players who hold this land.
	 * @param decLand. Decrease the occupiedTerritory of the player about the amount 1
	 * @param incLand. Increase the occupiedTerritory of the player about the amount 1
	 */
	def setNewOccupiedTerritory(incLand:Land, decLand:Land )
	{
	  getAvatar(decLand.holder).occupiedTerritory -= 1
	  getAvatar(incLand.holder).occupiedTerritory += 1
	}
	
	def sendPlayerConfigMessage(playerCount:Int, botCount:Int):Boolean = 
	{
	  var correct = false
	  if(playerCount > 3)
    	    sendNotificationMessage(Message.Error,"Es koennen hoechsten drei Spieler mitspielen.")
    	  else if(playerCount < 2)
    	     sendNotificationMessage(Message.Error,"Es werden mindestens zwei Spieler benoetigt. ")
    	  else if(botCount > playerCount)
    	    sendNotificationMessage(Message.Error,"Die Anzahl der gesamten Spieler darf nicht kleiner sein als die Anzahl der Bots.")
    	  else if(botCount == playerCount)
    	    sendNotificationMessage(Message.Error,"Zu viele Bots ausgewaehlt, es muss mindestens eine Person mitspielen.")
    	  else
    	    correct = true
    	 
    correct
	}
	
	/**
	 * Increase and decrease the units about the assigned amount for the attack- and defense-land.
	 * @param army. Number of units
	 */
	def setArmyForAttackAndDefenseLand(attack:Land, otherLand:Land, army:Int) 
	{
		 attack.setArmy(attack.getArmy - army)
		 otherLand.setArmy(otherLand.getArmy + army)
	}
	
	
		/**
	 * Set the new holder which is the attack land for the beaten land.
	 */
	def setNewHolderForBeatenLand(winLand:Land, lostLand:Land) = lostLand.holder = winLand.holder
	
	
	/**
	 * Set all requiered values after a successfully singleattack for  the loser and winner.
	 * @param winLand. Current player Land which is responsible for the attack.
	 * @param lostLand. Enemy Land which is now a new own land for the owner of the attack land.
	 */
	def setValueForWinnerAndLoser(winLand: Land, lostLand: Land)
	{
		    	setNewOccupiedTerritory(winLand, lostLand)
		        setNewHolderForBeatenLand(winLand, lostLand)
	}
}

//	


//	




//	
//	



//
//	initWorld
//	
//	//phase meaning 1 => Reinforcement, 2=>Battle, 3=>Tactic
//	//var phase:Int = -1 

//	
//	def testBotInitPlayer():Array[Avatar] =
//	{
//		var avatarContainer = new Array[Avatar](2) 
//
//	    avatarContainer(0) = new Avatar(0)
//	    avatarContainer(0).color = Avatar.colorContainer(0)
//	    
//	    avatarContainer(1) = new Bot(1, this)
//	    avatarContainer(1).color = Avatar.colorContainer(1)
//	  
//	    avatarContainer
//	}
//	
//	
//	
//	def checkGameOver:Boolean =
//	{
//	 
//	  var numberOfActivePlayer = avatarContainer.filterNot(player=> player.lost).length
//	  if(numberOfActivePlayer == 1)
//	  {
//		
//	    true 
//	  }
//	    else
//	      false
//	}
//	
//	

//	
//	def startShowGameMenu
//	{
//	  var notification = new Notification(Notification.Menu)
//	  notifyObservers(notification);
//	}
//	
//	def startShowHelp
//	{
//	  var notification = new Notification(Notification.Help)
//	  notifyObservers(notification);
//	}
//	
//	

//	
//	

//	
//	

//	

//	

//	

//	
//	
////	/**
////	 * Set a chose amount of Unit to chose field and to an assigned player.
////	 * @param player who makes a reinforcement.
////	 */
////	def startReinforcement(player: Avatar)=
////	{
////	    sendNotificationMessage(Message.Info, "                    Verstaerkungsphase!")
////	    sendNotificationUI
////	    
//////	   Raus löschen nur zum testen gedacht
////	    sendNotificationMessage(Message.Info, "Moechtest du einen Verstärkung? (ja/nein)")
////		  var n = new Notification(Notification.Question)
////		  n.currentPlayer = player
////		  notifyObservers(n)
////	    if(player.myTurn)
////	    {
////	      
////	    
////		player.newUnitsTemporary = player.getTerritories/Avatar.divider
////		if (player.newUnitsTemporary < 3) player.newUnitsTemporary = 3;
////		do{
////		    sendNotificationPlayerMessage(player,"Spieler: " + player.id)
////		    sendNotificationMessage(Message.Info," ist dran.")
////		    sendNotificationMessage(Message.Info,"Zahl der Verstaerkung: "+ player.newUnitsTemporary)
////		    sendNotificationMessage(Message.Info,"Bitte Teritorium eingeben (Spalte,Zeile).")
////			var notification = new Notification(Notification.Reinforcement)
////			notification.value = player.newUnitsTemporary
////			notification.currentPlayer = player
////		    notifyObservers(notification);
////		}while(player.newUnitsTemporary != 0)
////	    }
////	}
//	

//	
//	
//	  
//	  
//	

//	

//	
//	

//	

//	
//	/**
//	 * Set the attack or defense land in game. For an invalid position which is correlate with the player and the isOwnLand flag, the
//	 * user will get a failure message.
//	 * @param position. World Position in the game.
//	 * @param isOwnLand. For true the function will set the value for the attack land otherwise the defense land.
//	 */
//	def setAttackOrDefenseLand(position:WorldPosition, isOwnLand:Boolean)
//	{
//	  if(isOwnLand)   
//		  fromLand = world(position.row) (position.column)
//	  else
//	      toLand = world(position.row) (position.column)
//	
//	}
//	

//	
//	/**
//	 * Send a Message with the help of the given parameter for the assigned land.
//	 * For an invalid position which is correlate with the player and the isOwnLand flag, the
//	 * user will get a failure message.
//	 * @param player. Should be the current player of the game.
//	 * @param position. World Position in the game.
//	 * @param isOwnLand. For true the function will set the value for the attack land otherwise the defense land
//	 */
//	def sendBattleAssignMessage(player:Avatar, position:WorldPosition, isOwnLand:Boolean)
//	{
//	  if(isOwnLand)
//	  {
//	    if(!checkOwnLandSelection(player, position) && !checkWaterLand(position))
//	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt.")
//	    else if(!checkOwnLandSelection(player, position) && checkWaterLand(position))
//	      sendNotificationMessage(Message.Error,"Wasser kann nicht gewaehlt werden.")
//	    else if(!checkEnoughArmy(position))
//	      sendNotificationMessage(Message.Error,"Das Land benoetigt mehr als nur eine Einheit fuer einen Angriff.")
//	    else if(!checkHasEnemyNeighbour(player, position))
//			sendNotificationMessage(Message.Error,"Das Land verfuegt ueber keine feindlichen Nachbarn. Waehle ein anderes Land.")
//		else
//			sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
//			  
//	  }
//	  if(!isOwnLand)
//	  {
//	    if(!fromLand.checkNeighbourhood(world(position.row)(position.column)))
//	    sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland, bitte wiederhole die Eingabe korrekt.")
//	    else if(world(position.row)(position.column).getHolder == -1)
//	      sendNotificationMessage(Message.Error,"Wasserfelder koennen nicht ausgewaehlt werden, bitte wiederhole die Eingabe korrekt.")
//	      else if(world(position.row)(position.column).checkHolder(player))
//	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein feindliches Land, bitte wiederhole die Eingabe korrekt.")
//	    else
//	    {
//	    	sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
//	    }
//	
//	  }
//	}
//	
//		
//	/**
//	 * Send Battle and information notifications.
//	 * @param player. Should be the current player of the game.
//	 * @param ownLand. Notifications for the own land of the player or foreign land.
//	 */
//	def sendBattleNotification(player:Avatar, ownLand:Boolean)
//	{
//		var notification = new Notification(Notification.BattleAssign)
//	  
//		if(ownLand)
//			sendNotificationMessage(Message.Info, "Waehle das Land, von welchem du aus Angreifen willst.")
//		else
//			sendNotificationMessage(Message.Info,"Waehle das Land aus, welches du Angreifen willst.")
//	      
//		notification.currentPlayer = player
//		notification.isFromLand = ownLand
//	  
//		notifyObservers(notification);
//	    
//	}
//	
////	/**
////	 * Manage the count of the attacks and communicate with the user about it.
////	 * @param player. Should be the current player of the game.
////	 */
////	def attack (player:Avatar, ownLand:Land, otherLand:Land)
////	{
////
////	  var outloop = false
////	  
////	  sendNotificationMessage(Message.Info,"Angreifen? (ja/nein)")
////      sendInputRequest
////	  
////	  while(!outloop && player.answer)
////		  {
////		  	
////	    
////		    if(ownLand.getArmy > 1)
////		    {
////			    singleAttack(ownLand, otherLand)
////			    
////			    if(otherLand.checkHolder(player) || gameOver)
////			    {
////			      println("Debug (attack): otherland holder == player " )
////			      outloop = true
////			    }
////			    else if (ownLand.getArmy != 1){
////			    	sendNotificationPlayerMessage(player, "Spieler " + player.id)
////			    	sendNotificationMessage(Message.Info, ": Noch verbliebene Einheiten: " + ownLand.getArmy)
////			    	
////			    	sendNotificationPlayerMessage(avatarContainer(otherLand.getHolder), "Spieler " + otherLand.getHolder)
////			    	sendNotificationMessage(Message.Info, ": Noch verbliebene Einheiten: " + otherLand.getArmy)
////
////			    	sendNotificationMessage(Message.Info,"Weiter angreifen? (ja/nein)")
////			    	sendInputRequest
////					if(!currentPlayer.answer)
////					{
////						outloop = true
////					}
////					else
////					{
////						outloop = false
////					}
////				}
////			    else 
////			    {
////			      sendNotificationPlayerMessage(player,"Spieler " + player.id)
////			      sendNotificationMessage(Message.Info,": Angriff gescheitert")
////			    }
////		    }
////		    else
////		      outloop = true			  	
////		  }
////	    resetFromAndToLand
////	    if(checkPossibleToBattle(player) && !gameOver)
////	    {
////	    	sendNotificationMessage(Message.Info,"Ein weiteres Land angreifen? (ja/nein)")
////	  		val notification = new Notification(Notification.Question)
////	      	notification.currentPlayer = player
////	      	notifyObservers(notification)
////	    }else if(!gameOver)
////	    {
////	     // player.myTurn = false
////	      sendNotificationMessage(Message.Info,"Kein weiterer Zug mehr moeglich.")
////	    }     
////	}
//	

//	

//	
//
//	

//	
//	

//	
//	

//	
//	

//	

//	
//	

//	

//	

//	

//	

//	
//	

//	
//	
//	/**
//	 * Check if the player achieve the conditions for the TacticPhase
//	 * @param player. player to validate.
//	 * @return Is it possible to battle the function return true. 
//	 */
//	def checkPossibleForTactic(player:Avatar):Boolean =
//	{
//	  var isTacticPossible = false
//	  var possibleTacticLandContainer = fieldContainer.filter(field => field.checkHolder(player))
//	  if(possibleTacticLandContainer.length != 0)
//	  {
//	    possibleTacticLandContainer =  possibleTacticLandContainer.filter(field=>checkTacticLandSelection(player,field.position,true))
//	  }
//	  
//	  if(possibleTacticLandContainer.length != 0 )
//	    isTacticPossible = true
//	  else
//	  {
//	    isTacticPossible = false
//	    
//	  }
//	  
//	  isTacticPossible
//	}
//	
//	

//	
//	/**
//	 * Set the from or to land.
//	 * Inform the user about the result of the given position.
//	 * @param player. Should be the current player of the game.
//	 * @param position. Position of the game world.
//	 * @param isFrom. To set the from (Source) land the parameter require the value true, otherwise false for the to (Target) land.  
//	 */
//	def setFromOrTo(player:Avatar, position:WorldPosition,isFrom:Boolean)
//	{
//	       if(isFrom)
//	        fromLand = world(position.row)(position.column)   
//	      else
//	        toLand = world(position.row)(position.column)
//
//	}
//	
//	/**
//	 * Send notifications to get 'from' or to 'land' of the user.
//	 * @param player. Should be the current player of the game.
//	 * @param from. Give true when you require the source land otherwise false for the target land.
//	 */
//	def sendTacticNotification(player:Avatar,from:Boolean)
//	{
//		var notification = new Notification(Notification.TacticAssign)
//	  
//		if(from)
//		{
//			sendNotificationMessage(Message.Info,"Bitte waehle ein eigenes Land aus um Truppen zu verschieben")
//			notification.isFromLand = true
//		}
//			
//		else
//		{
//			sendNotificationMessage(Message.Info,"Bitte Land angeben auf das verschoben werden soll")
//			notification.isFromLand = false
//		}
//		notification.currentPlayer = player
//	    
//		notifyObservers(notification);
//	    
//	}
//	

//	
//	
//	/**
//	 * Send a Message with the help of the given parameter for the assigned land.
//	 * For an invalid position which is correlate with the player and the isOwnLand flag, the
//	 * user will get a failure message.
//	 * @param player. Should be the current player of the game.
//	 * @param position. World Position in the game.
//	 * @param isFrom. For true the function will set the value for the from land otherwise the to land
//	 */
//	def sendTacticAssignMessage(player:Avatar, position:WorldPosition, isFrom:Boolean)
//	{
//	  if(isFrom)
//	  {
//	    if(!checkOwnLandSelection(player, position) && !checkWaterLand(position))
//	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt.")
//	    else if(!checkOwnLandSelection(player, position) && checkWaterLand(position))
//	      sendNotificationMessage(Message.Error,"Wasser kann nicht gewaehlt werden.")
//	    else if(!checkEnoughArmy(position))
//	      sendNotificationMessage(Message.Error,"Das Land benoetigt mehr als nur eine Einheit fuer einen Angriff.")
//	    else if(!checkHasOwnNeighbour(player, position))
//			sendNotificationMessage(Message.Error,"Das Land verfuegt ueber keine Nachbarn die zu dir gehoeren. Waehle ein anderes Land.")
//		else
//			sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
//			  
//	  }
//	  if(!isFrom)
//	  {
//	    if(!fromLand.checkNeighbourhood(world(position.row)(position.column)))
//	    sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland, bitte wiederhole die Eingabe korrekt.")
//	    else if(world(position.row)(position.column).getHolder == -1)
//	      sendNotificationMessage(Message.Error,"Wasserfelder koennen nicht ausgewaehlt werden, bitte wiederhole die Eingabe korrekt.")
//	      else if(!world(position.row)(position.column).checkHolder(player))
//	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein eigenes Land, bitte wiederhole die Eingabe korrekt.")
//	    else
//	    {
//	    	sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
//	    }
//	  }
//	}
//	
//

//	
//
//	/**
//	 * Move units from source-land to the target land for the assigned player.
//	 * Inform the User about the status and rules of the move
//	 * @param from. Source land which can units move to the target land.
//	 * @param to. Target land which receive Units of the source land.
//	 * @param player. Should be the current player of the game.
//	 */
//	def setArmyToMove(from: Land, to: Land, player:Avatar)
//	{  
//	    	   do
//	    	   {
//		    	   sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
//		    	   sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
//	    	       sendNotificationMessage(Message.Info,"Einheiten: " + from.getArmy)
//	    	       var n = new Notification(Notification.TacticArmy)
//		    	   n.currentPlayer = player
//		    	   notifyObservers(n);
//	    	   }while (!from.permissionMoveArmy)
//		    	 
//	    	     for(i <- 0 until player.newUnitsTemporary)
//	    	     {
//	    	       from.decArmy
//	    	       to.incArmy
//	    	     }
//	    	 player.newUnitsTemporary = 0    
//	    	 
//	 }
//	     
//	     	
// 	def newGame
// 	{
// 	  	fieldContainer= null
// 		fromLand = null
// 		toLand = null
// 		gameOver = false
// 	}

//	  		else
//			{
//				sendNotificationMessage(Message.Info, "Waehle das Land aus, welches du Angreifen willst.")
//				currentInputType = "position"
//				sendInputRequest
//				if(toLand != null)
//					attack
//				else
//					gameHandler
//			}
//		}
//	}

//	
//	
//	def setToLandSelection(pos:WorldPosition, ownFlag: Boolean)
//	{
//	  if(!ownFlag)
//	  {
//	  }
//	  else
//	  {
//	    if(checkTacticLandSelection(pos))
//	      toLand = world(pos.row)(pos.column)
//	  }
//	}
//	
//	
//	/**
//	 * Validate whether the current player is owner of the land.
//	 * @param player. Should be the current player.
//	 * @param position. World position of the land.
//	 * @return true when the selected land is from the current player.
//	 */
//	def checkOwnLandSelection(pos:WorldPosition):Boolean =
//	{
//		var ownLand = world(pos.row)(pos.column)
//		var ok = false
//		if(ownLand.checkHolder(currentPlayer))
//			ok = true
//		else
//			ok = false
//		ok
//	}
//	
//	
//	def checkEnemyLandSelection(position:WorldPosition):Boolean =
//	{
//		var otherLand = world(position.row)(position.column)
//		var ownLand = fromLand
//		var ok = false
//		if(ownLand.checkNeighbourhood(otherLand) && !otherLand.checkHolder(currentPlayer) && otherLand.getHolder != -1)
//			ok = true
//		else
//			ok = false
//		ok		 
//	}
//	
//	
//	/**
//	 * Check if the selected land for the tactic process is valid.
//	 * @param player. Should be the current player of the game.
//	 * @param position. The position of the selected land.  
//	 */
//	def checkTacticLandSelection(position:WorldPosition):Boolean =
//	{
//	  var ok = false
//	  if(checkOwnLandSelection(currentPlayer, position) && fromLand.checkNeighbourhood(world(position.row)(position.column)) )
//	    	ok = true
//	  else
//	    	ok = false
//	  ok
//	}
//	
//    
//    /*
//	 * Taktische Phase der Spieler hat einmal das Recht
//	 * seine Einheiten von max. einem Land zu einem anderen Land,
//	 * das ihn gehoert, zu verschieben.
//	 * Danach ist die Runde fuer den Spieler Beendet und der 
//	 * Naechste ist an der Reihe.
//	 * */
//	def startTacticPhase
//	{  
//	  if(checkPossibleForTactic(currentPlayer))
//	  {
//	      sendNotificationMessage(Message.Info,"Taktische Phase!\nMoechtest du Einheiten verschieben? (ja/nein)")
//		  val notification = new Notification(Notification.Question)
//		  notification.currentPlayer = currentPlayer
//		  notifyObservers(notification)
//		  // falls ja fuehre Phase aus, ansonsten Zug beendet
//		  if(currentPlayer.answer)
//		  {	
//			  sendNotificationUI
//			    do
//			    {
//			      sendTacticNotification(currentPlayer,true)
//			    }while(!currentPlayer.inputCorrect)
//			      
//			    do
//			    {
//			      sendTacticNotification(currentPlayer,false)
//			    }while(!currentPlayer.inputCorrect)
//			    
//			    setArmyToMove(fromLand, toLand, currentPlayer)
//			    sendNotificationUI
//			    resetFromAndToLand
//
//		  }
//	  }else
//	  {
//	    sendNotificationMessage(Message.Info, "Taktische Phase wird ausgesetzt, da kein Land mit ausreichend Einheiten an ein eigenes Nachbarland angrenzt .")
//	  }
//	}
//}