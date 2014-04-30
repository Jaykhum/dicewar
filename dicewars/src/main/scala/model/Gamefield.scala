package main.scala.model

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.collection.immutable.StringOps
import main.scala.util.FileUtil
import scala.util.Random
import main.scala.util._
import main.scala.util.Message

class Gamefield extends Observable{
	val divider = 3
	val height = 10;
	val width = 18;
	val playercount = 2;
	val avatarContainer:Array[Avatar] = initPlayer(2)
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](height,width)
	var fieldContainer:Array[Field] = null
	// path to Maps
	val mapDir:String = "Maps/"

	var fromLand:Land = null
	var toLand:Land = null
	/**
	 * Initialize number of players.
	 * @param numberofPlayer. Number of player.
	 */
	def initPlayer(numberofPlayer:Integer):Array[Avatar] =
	{
	  var avatarContainer = new Array[Avatar](numberofPlayer) 
	  for(i <- 0 to numberofPlayer -1)
	  {
	    avatarContainer(i) = new Avatar(i)
	  }
	  avatarContainer
	}
	
	def startShowGameMenu
	{
	  var notification = new Notification(Notification.Menu)
	  notifyObservers(notification);
	}
	
	def startShowHelp
	{
	  var notification = new Notification(Notification.Help)
	  notifyObservers(notification);
	}
	
	def startShowMapExample
	{
	  var notification = new Notification(Notification.MapSample)
	  notifyObservers(notification);
	}
	
	/**
	 * Initialize the world as a 18x10 game matrix with waterfields.
	 * */
	def initWorld
	{
	  for(i <- 0 to height-1; j <- 0 to width-1)
	  {
	    world(i)(j) = new Water(i,j)
	  }
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
	 * Send Notification to every UI to redraw the UI.
	 */
	def sendNotificationUI()
	{
	  var notification = new Notification(Notification.DrawUI)
	  notifyObservers(notification);
	}
	
	/**
	 * Assigns player to created Fields.
	 * @param require an Array with all Fields
	 */
	def initFieldHolder(fieldContainer:Array[Field])
	{
	  //TODO Anpassen auf mehr als nur zwei Spieler.
	  val rnd = new Random(playercount)
	  var playerID =  0
	  var playerFieldCount = new Array[Int](2)
	  var fieldCount:Int = fieldContainer.size
	  playerFieldCount(0) = 0
	  playerFieldCount(1) = 0
	  for(i <- 0 to fieldContainer.size -1)
	  {
		 playerID =  rnd.nextInt(playercount)
		  
		  if( (playerFieldCount(0)+1) > (fieldCount/2) )
		  {
		    playerID = 1  
		  }else if( (playerFieldCount(1)+1) > (fieldCount/2) )
		  {
		    playerID = 0 
		  }
		  
		  world(fieldContainer(i).position.row) (fieldContainer(i).position.column).setHolder(playerID)
		  playerFieldCount(playerID) += 1
	  }
	  for(i <- 0 to avatarContainer.size -1)
	  {
	    avatarContainer(i).occupiedTerritory = playerFieldCount(i) 
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
		for(a <- 0 to outArray.size-1)
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
	 * Set a chose amount of Unit to chose field and to an assigned player.
	 * @param player who makes a reinforcement.
	 */
	def startReinforcement(player: Avatar)={
	    sendNotificationMessage(Message.Info, "Verstaerkungsphase!")
	    
		player.newUnitsTemporary = player.getTerritories/divider
		if (player.newUnitsTemporary < 3) player.newUnitsTemporary = 3;
		do{
		    sendNotificationMessage(Message.Info,"Spieler: " + player.id + " ist dran.")
		    sendNotificationMessage(Message.Info,"Zahl der Verstaerkung: "+ player.newUnitsTemporary)
		    sendNotificationMessage(Message.Info,"Bitte Teritorium eingeben (Spalte,Zeile).")
			var notification = new Notification(Notification.Reinforcement)
			notification.value = player.newUnitsTemporary
			notification.currentPlayer = player
		    notifyObservers(notification);
		}while(player.newUnitsTemporary != 0)
	}
	
	/**
	 * Check whether the reinforcement is correct, otherwise the user get an appropriate message.
	 * @param player. Current player for the reinforcement.
	 * @param position. Land Position for the Reinforcement.
	 */
	def handleReinforcement(player:Avatar,position:WorldPosition) 
	{
	  // Spielfeld Land durch Pos bestimmen und checken ob seins
	  if(world(position.row)(position.column).checkHolder(player) && world(position.row)(position.column).getFieldType)
			{
			  world(position.row)(position.column).incArmy
			  player.newUnitsTemporary -= 1
			  sendNotificationUI
			}
			else if(!world(position.row)(position.column).getFieldType)
			  sendNotificationMessage(Message.Error,"Ein Wasserfeld hat kein besitzer")
			else
			  sendNotificationMessage(Message.Error,"Das Teritorium ist nicht dein Land.")
	}
	
	
	/**
	 * Set the new holder which is the attack land for the beaten land.
	 */
	def setNewHolderForBeatenLand = toLand.holder = fromLand.holder
	
	/**
	 * Check whether the amount of unit to move for the assigned land is allowed.
	 * When the amount is too high, the user will be informed about it.
	 * @param land. The army of this land be checked. 
	 * @param army. Number of desired units.
	 * @return when the assigned values is allowed the function will return true otherwise false.
	 */
	def checkNumberOfUnitMove(land:Land, army:Int):Boolean =
	{
		if(land.getArmy - army < 1)
		{
			sendNotificationMessage(Message.Error,"Zuviele Einheiten gewaehlt. Bitte erneut eingeben.")
			return false
		}else if(army == 0)
		{
		  sendNotificationMessage(Message.Error,"Mindestens eine Einheit muss verschoben werden. Bitte erneut eingeben.")
		  return false
		}
	    else
	    	return true
	    false
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
	  notifyObservers(notificationMessage)
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
	 * Check is land in the world a water type, return true when the land type is water.
	 * @param position. Position in the world
	 */
	def checkWaterLand(position:WorldPosition):Boolean =
	{
	  if(!world(position.row)(position.column).getFieldType)
	    true
	    else
	      false
	}
	
	/**
	 * Set the attack or defense land in game. For an invalid position which is correlate with the player and the isOwnLand flag, the
	 * user will get a failure message.
	 * @param position. World Position in the game.
	 * @param isOwnLand. For true the function will set the value for the attack land otherwise the defense land.
	 */
	def setAttackOrDefenseLand(position:WorldPosition, isOwnLand:Boolean)
	{
	  if(isOwnLand)   
		  fromLand = world(position.row) (position.column)
	  else
	      toLand = world(position.row) (position.column)
	
	}
	
	/**
	 * Check whether the land has enough army to attack or to move.
	 * @param position. Position of the world.
	 */
	def checkEnoughArmy(position:WorldPosition):Boolean =
	{
	  var ok:Boolean = false
	  if(world(position.row)(position.column).getArmy >1)
	      ok = true
	  ok
	}
	
	/**
	 * Send a Message with the help of the given parameter for the assigned land.
	 * For an invalid position which is correlate with the player and the isOwnLand flag, the
	 * user will get a failure message.
	 * @param player. Should be the current player of the game.
	 * @param position. World Position in the game.
	 * @param isOwnLand. For true the function will set the value for the attack land otherwise the defense land
	 */
	def sendBattleAssignMessage(player:Avatar, position:WorldPosition, isOwnLand:Boolean)
	{
	  if(isOwnLand)
	  {
	    if(!checkOwnLandSelection(player, position) && !checkWaterLand(position))
	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt.")
	    else if(!checkOwnLandSelection(player, position) && checkWaterLand(position))
	      sendNotificationMessage(Message.Error,"Wasser kann nicht gewaehlt werden.")
	    else if(!checkEnoughArmy(position))
	      sendNotificationMessage(Message.Error,"Das Land benoetigt mehr als nur eine Einheit fuer einen Angriff.")
	    else if(!checkHasEnemyNeighbour(player, position))
			sendNotificationMessage(Message.Error,"Das Land verfuegt ueber keine feindlichen Nachbarn. Waehle ein anderes Land.")
		else
			sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
			  
	  }
	  if(!isOwnLand)
	  {
	    if(!fromLand.checkNeighbourhood(world(position.row)(position.column)))
	    sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland, bitte wiederhole die Eingabe korrekt.")
	    else if(world(position.row)(position.column).getHolder == -1)
	      sendNotificationMessage(Message.Error,"Wasserfelder koennen nicht ausgewaehlt werden, bitte wiederhole die Eingabe korrekt.")
	      else if(world(position.row)(position.column).checkHolder(player))
	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein feindliches Land, bitte wiederhole die Eingabe korrekt.")
	    else
	    {
	    	sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
	    }
	
	  }
	}
	
		
	/**
	 * Start the Battle Phase for the assigned player.
	 * @param player. Should be the current player.
	 */
	def startBattlePhase(player:Avatar)
	{
	  if(checkPossibleToBattle(player))
	  {   
		  sendNotificationMessage(Message.Info, "Battle Phase!\nMoechtest du einen Kampf? (ja/nein)")
		  var n = new Notification(Notification.Question)
		  n.currentPlayer = player
		  notifyObservers(n)
		  if(player.myTurn)
		  {
		    
			  do
			  {
			    sendNotificationUI
			    do
			    {
			      sendBattleNotification(player,true)
			    }while(!player.inputCorrect)
			      
			    do
			    {
			      sendBattleNotification(player,false)
			    }while(!player.inputCorrect)
			      
			    attack(player)
			  }while(player.myTurn)
		  }
	  }else
	  {
	    sendNotificationMessage(Message.Info, "Battle Phase wird ausgesetzt, da nicht genug Einheiten auf eigene Laender stationiert sind die an feindliche Nachbarn grenzen .")
	  }
	}
	
	/**
	 * Send Battle and information notifications.
	 * @param player. Should be the current player of the game.
	 * @param ownLand. Notifications for the own land of the player or foreign land.
	 */
	def sendBattleNotification(player:Avatar, ownLand:Boolean)
	{
		var notification = new Notification(Notification.BattleAssign)
	  
		if(ownLand)
			sendNotificationMessage(Message.Info, "Waehle das Land, von welchem du aus Angreifen willst.")
		else
			sendNotificationMessage(Message.Info,"Waehle das Land aus welches du Angreifen willst.")
	      
		notification.currentPlayer = player
		notification.isFromLand = ownLand
	  
		notifyObservers(notification);
	    
	}
	
	/**
	 * Manage the count of the attacks and communicate with the user about it.
	 * @param player. Should be the current player.
	 */
	def attack (player:Avatar)
	{
	  var ownLand = fromLand
	  var otherLand = toLand
	  var outloop = false
	  
	  sendNotificationMessage(Message.Info,"Angreifen? (ja/nein)")
	  val n = new Notification(Notification.Question)
	  n.currentPlayer = player
	  notifyObservers(n)
	  
	  while(!outloop && player.myTurn)
		  {
		  	
	    
		    if(ownLand.getArmy > 1)
		    {
			    singleAttack(ownLand, otherLand)
			    if(otherLand.checkHolder(player))
			    {
			      println("Debug (attack): otherland holder == player " )
			      outloop = true
			    }
			    else if (ownLand.getArmy != 1){
			    	sendNotificationMessage(Message.Info,"Spieler " + player.id + ": Noch verbliebene Einheiten: " + ownLand.getArmy)
			    	sendNotificationMessage(Message.Info,"Spieler " + otherLand.getHolder + ": Noch verbliebene Einheiten: " + otherLand.getArmy)
			    	sendNotificationMessage(Message.Info,"Weiter angreifen? (ja/nein)")
			    	val notification = new Notification(Notification.Question)
			    	notification.currentPlayer = player
			    	notifyObservers(notification)
			    	println("player Turn: " + player.myTurn )
					if(!player.myTurn)
					{
						outloop = true
					}
					else
					{
						outloop = false
					}
				}
			    else 
			    {
			      sendNotificationMessage(Message.Error,"Spieler " + player.id + ": Angriff gescheitert")
			    }
		    }
		    else
		      outloop = true			  	
		  }
	    resetFromAndToLand
	    if(checkPossibleToBattle(player))
	    {
	    	sendNotificationMessage(Message.Info,"Ein weiteres Land angreifen? (ja/nein)")
	  		val notification = new Notification(Notification.Question)
	      	notification.currentPlayer = player
	      	notifyObservers(notification)
	    }else
	    {
	      player.myTurn = false
	      sendNotificationMessage(Message.Info,"Kein weiterer Zug mehr möglich.")
	    }
	      
	}
	
	/**
	 * Check if the player achieve the conditions to Battle
	 * @param player. player to validate.
	 * * @return Is it possible to battle the function return true. 
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
	
	/**
	 * Reset the attribute fromLand and toLand.
	 */
	def resetFromAndToLand
	{
	  fromLand = null
	  toLand = null
	}
	
	/**
	 * Execute a single Attack. In case the chose lands was invalid the user will get an appropriate message.
	 * @param attack. Current player Land which is responsible for the attack.
	 * @param defense. Foreign Land which try to defend.
	 */
	def singleAttack(attack: Land, defense: Land ) ={
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
		     for(i <- 0 to attackDice.length -1)
		     {
		       attackDice(i) = dice.roll
		       sendNotificationMessage(Message.Info,"Spieler " + attack.getHolder +" hat eine " + attackDice(i) + " gewuerfelt. ")
	//	       println("Wuerfel Augen: " + attackDice(i))
		     }
		     for(i <- 0 to defenseDice.length -1)
		     {
		       defenseDice(i) = dice.roll
		       sendNotificationMessage(Message.Info,"Spieler " + defense.getHolder +" hat eine " + defenseDice(i) + " gewuerfelt. ")
	//	       println("Wuerfel Augen: " + defenseDice(i))
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
		    	moveUnit(attack,defense,attackCountDices)
		    	sendNotificationUI
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
	 * Move the units from attack-land to the defense land with an assigned amount of units.
	 * Inform the User about the status and rules of the move
	 * @param attack. Current player Land which is responsible for the attack.
	 * @param defense. Former Foreign Land which is now a new own land for the owner of the attack land.
	 * @param attackCountDices. Number of Units to move.
	 */
	def moveUnit(attack: Land, defense: Land, attackCountDices:Int)
	{  
	    	 sendNotificationMessage(Message.Success,"Sieg!!")
	    	 if(attack.getArmy > 3)
	    	 {
	    	   do
	    	   {
		    	   sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		    	   sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
	    	       sendNotificationMessage(Message.Info,"Spieler "+ attack.getHolder + ":  Noch verbliebene Einheiten: " + attack.getArmy)
	    	       var n = new Notification(Notification.BattleAttack)
		    	   notifyObservers(n);
	    	   }while (!attack.permissionMoveArmy)
		    	 
	    	 }
	    	 else{ 
	    	     attack.permissionMoveArmy = true
		    	 setValueForAttackAndDefenseLand(attackCountDices)
	    	 }
	    	 
	    
	  attack.permissionMoveArmy = false
	}
	
	
	/**
	 * Increase and decrease the units about the assigned amount for the attack- and defense-land.
	 * Preconditioned it is allowed.
	 */
	def setValueForAttackAndDefenseLand(army:Int)
	{
	   if(fromLand.permissionMoveArmy)
      {
        setArmyForAttackAndDefenseLand(army)
        setNewHolderForBeatenLand
      }
	}
	
	/**
	 * Increase and decrease the units about the assigned amount for the attack- and defense-land.
	 * @param army. Number of units
	 */
	def setArmyForAttackAndDefenseLand(army:Int) 
	{
		 fromLand.setArmy(fromLand.getArmy - army)
		 toLand.setArmy(toLand.getArmy + army)
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
	  for(i <- 0 to neighbourContainer.length-1)
	  {
	    if(!checkPositionIsInWorld(neighbourContainer.apply(i)))
	    	 neighbourContainer -= neighbourContainer.apply(i)
	  }
	  if(neighbourContainer.length != 0)
	  {
	    for(i <- 0 to neighbourContainer.length-1)
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
	
	def checkPositionIsInWorld(position:WorldPosition):Boolean =
	{
	  if(position.row < 0 || position.row > height-1 || position.column < 0 || position.column > width -1)
	    false
	  else
	    true
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
		if(ownLand.checkHolder(player))
			ok = true
		else
			ok = false
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
		var ownLand = world(fromLand.position.row)(fromLand.position.column)
		var ok = false
		if(ownLand.checkNeighbourhood(otherLand) && !otherLand.checkHolder(player) && otherLand.getHolder != -1)
			ok = true
		else
			ok = false
		ok		 
	}
	
	
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
	 * Check if the player achieve the conditions for the TacticPhase
	 * @param player. player to validate.
	 * @return Is it possible to battle the function return true. 
	 */
	def checkPossibleForTactic(player:Avatar):Boolean =
	{
	  var isTacticPossible = false
	  var possibleTacticLandContainer = fieldContainer.filter(field => field.checkHolder(player))
	  if(possibleTacticLandContainer.length != 0)
	  {
	    possibleTacticLandContainer =  possibleTacticLandContainer.filter(field=>checkTacticLandSelection(player,field.position,true))
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
	 * Taktische Phase der Spieler hat einmal das Recht
	 * seine Einheiten von max. einem Land zu einem anderen Land,
	 * das ihn gehoert, zu verschieben.
	 * Danach ist die Runde fuer den Spieler Beendet und der 
	 * Naechste ist an der Reihe.
	 * */
	def startTacticPhase(player:Avatar)
	{
	  
	  if(checkPossibleForTactic(player))
	  {
	      sendNotificationMessage(Message.Info,"Taktische Phase!\nMoechtest du Einheiten verschieben? (ja/nein)")
		  val notification = new Notification(Notification.Question)
		  notification.currentPlayer = player
		  notifyObservers(notification)
		  // falls ja fuehre Phase aus, ansonsten Zug beendet
		  if(player.myTurn)
		  {	
			  sendNotificationUI
			    do
			    {
			      sendTacticNotification(player,true)
			    }while(!player.inputCorrect)
			      
			    do
			    {
			      sendTacticNotification(player,false)
			    }while(!player.inputCorrect)
			    
			    setArmyToMove(fromLand, toLand, player)
			    sendNotificationUI
			    resetFromAndToLand

		  }
	  }else
	  {
	    sendNotificationMessage(Message.Info, "Taktische Phase wird ausgesetzt, da kein Land mit ausreichend Einheiten an ein eigenes Nachbarland angrenzt .")
	  }
		 
	}
	
	/**
	 * Set the from or to land.
	 * Inform the user about the result of the given position.
	 * @param player. Should be the current player of the game.
	 * @param position. Position of the game world.
	 * @param isFrom. To set the from (Source) land the parameter require the value true, otherwise false for the to (Target) land.  
	 */
	def setFromOrTo(player:Avatar, position:WorldPosition,isFrom:Boolean)
	{
	       if(isFrom)
	        fromLand = world(position.row)(position.column)   
	      else
	        toLand = world(position.row)(position.column)

	}
	
	/**
	 * Send notifications to get 'from' or to 'land' of the user.
	 * @param player. Should be the current player of the game.
	 * @param from. Give true when you require the source land otherwise false for the target land.
	 */
	def sendTacticNotification(player:Avatar,from:Boolean)
	{
		var notification = new Notification(Notification.TacticAssign)
	  
		if(from)
		{
			sendNotificationMessage(Message.Info,"Bitte waehle ein eigenes Land aus um Truppen zu verschieben")
			notification.isFromLand = true
		}
			
		else
		{
			sendNotificationMessage(Message.Info,"Bitte Land angeben auf das verschoben werden soll")
			notification.isFromLand = false
		}
		notification.currentPlayer = player
	    
		notifyObservers(notification);
	    
	}
	
	/**
	 * Check if the selected land for the tactic process is valid.
	 * @param player. Should be the current player of the game.
	 * @param position. The position of the selected land.
     * @param isFrom. The function require the value true to set the (source-) from-land, otherwise the (target-) to-land.  
	 */
	def checkTacticLandSelection(player:Avatar, position:WorldPosition, isFrom:Boolean):Boolean =
	{
	  var ok = false
	  
	  if(isFrom)
	  {
		  if(checkOwnLandSelection(player, position) && checkHasOwnNeighbour(player, position) && checkEnoughArmy(position))
		   ok = true
		  else
		   ok = false
	  }else
	  {
	    if(checkOwnLandSelection(player, position) && fromLand.checkNeighbourhood(world(position.row)(position.column)) )
	    	ok = true
	    else
	    	ok = false
	  }
	  
	  ok
	}
	
	
	/**
	 * Send a Message with the help of the given parameter for the assigned land.
	 * For an invalid position which is correlate with the player and the isOwnLand flag, the
	 * user will get a failure message.
	 * @param player. Should be the current player of the game.
	 * @param position. World Position in the game.
	 * @param isFrom. For true the function will set the value for the from land otherwise the to land
	 */
	def sendTacticAssignMessage(player:Avatar, position:WorldPosition, isFrom:Boolean)
	{
	  if(isFrom)
	  {
	    if(!checkOwnLandSelection(player, position) && !checkWaterLand(position))
	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt.")
	    else if(!checkOwnLandSelection(player, position) && checkWaterLand(position))
	      sendNotificationMessage(Message.Error,"Wasser kann nicht gewaehlt werden.")
	    else if(!checkEnoughArmy(position))
	      sendNotificationMessage(Message.Error,"Das Land benoetigt mehr als nur eine Einheit fuer einen Angriff.")
	    else if(!checkHasOwnNeighbour(player, position))
			sendNotificationMessage(Message.Error,"Das Land verfuegt ueber keine Nachbarn die zu dir gehoeren. Waehle ein anderes Land.")
		else
			sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
			  
	  }
	  if(!isFrom)
	  {
	    if(!fromLand.checkNeighbourhood(world(position.row)(position.column)))
	    sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist kein Nachbarland, bitte wiederhole die Eingabe korrekt.")
	    else if(world(position.row)(position.column).getHolder == -1)
	      sendNotificationMessage(Message.Error,"Wasserfelder koennen nicht ausgewaehlt werden, bitte wiederhole die Eingabe korrekt.")
	      else if(!world(position.row)(position.column).checkHolder(player))
	      sendNotificationMessage(Message.Error,"Das ausgewaehlte Land ist nicht dein eigenes Land, bitte wiederhole die Eingabe korrekt.")
	    else
	    {
	    	sendNotificationMessage(Message.Success,"Land wurde gewaehlt")
	    }
	
	  }
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
	  for(i <- 0 to neighbourContainer.length-1)
	  {
	    if(!checkPositionIsInWorld(neighbourContainer.apply(i)))
	    	 neighbourContainer -= neighbourContainer.apply(i)
	  }
	  if(neighbourContainer.length != 0)
	  {
	    for(i <- 0 to neighbourContainer.length-1)
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
	

	/**
	 * Move units from source-land to the target land for the assigned player.
	 * Inform the User about the status and rules of the move
	 * @param from. Source land which can units move to the target land.
	 * @param to. Target land which receive Units of the source land.
	 * @param player. Should be the current player of the game.
	 */
	def setArmyToMove(from: Land, to: Land, player:Avatar)
	{  
	    	   do
	    	   {
		    	   sendNotificationMessage(Message.Info,"Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		    	   sendNotificationMessage(Message.Info,"Hinweis: Eine Einheit muss stationiert bleiben.")
	    	       sendNotificationMessage(Message.Info,"Einheiten: " + from.getArmy)
	    	       var n = new Notification(Notification.TacticArmy)
		    	   n.currentPlayer = player
		    	   notifyObservers(n);
	    	   }while (!from.permissionMoveArmy)
		    	 
	    	     for(i <- 0 to player.newUnitsTemporary -1)
	    	     {
	    	       from.decArmy
	    	       to.incArmy
	    	     }
	    	 player.newUnitsTemporary = 0    
	    	 
	     }
}