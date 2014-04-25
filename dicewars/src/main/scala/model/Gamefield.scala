package main.scala.model

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import scala.collection.immutable.StringOps
import main.scala.util.FileUtil
import scala.util.Random
import main.scala.util._

class Gamefield extends Observable{
	val divider = 3
	val height = 10;
	val width = 18;
	val playercount = 2;
	val avatarContainer:Array[Avatar] = initPlayer(2)
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](height,width)
	// path to Maps
	val mapDir:String = "Maps/"

	var attackLand:Land = null
	var defenseLand:Land = null
	/**
	 * Initialize number of players.
	 * @param Number of player.
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
	
	/*
	 * Welt initialsieren als 18x10 Gamefeld und alle Felder default als Wasserfelder
	 * */
	def initWorld
	{
	  for(i <- 0 to height-1; j <- 0 to width-1)
	  {
	    world(i)(j) = new Water(i,j)
	  }
	}
	
	
	/**
	 * Initialize all Fields and they appended holder
	 */
	def initGame(map:String)
	{
	  var fieldContainer = initFieldInWorld(map)
	  initFieldHolder(fieldContainer)
	}
	
	
	/**
	 * Send Notification to every UI to redraw the UI.
	 */
	def sendNotificationUI()
	{
	  var notification = new Notification(Notification.UI)
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
		  
		  world(fieldContainer(i).position.row) (fieldContainer(i).position.colum).setHolder(playerID)
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
		  // colum

		  if(charArray.length == 3)	Position(1) = String.valueOf(charArray(2)).toInt
		  else if(charArray.length == 4)	Position(1) = String.valueOf(charArray(3)).toInt + 10
		  fieldContainer(a) = new Field(Position(0), Position(1)) 
		  world(Position(0)) (Position(1)) = fieldContainer(a)
		}
		fieldContainer
		
	}
	
	def getWorld() = world
	
	/**
	 * Set a chose amount of Unit to chose field and to an assigned player.
	 * @param player who makes a reinforcement.
	 */
	def startReinforcement(player: Avatar)={
		player.newUnitsTemporary = player.getTerritories/divider
		if (player.newUnitsTemporary < 3) player.newUnitsTemporary = 3;
		do{
		    sendNotificationInfo("Zahl der Verstaerkung: "+ player.newUnitsTemporary)
		    sendNotificationInfo("Spieler: " + player.id + " ist dran.")
		    sendNotificationInfo("Bitte Teritorium eingeben (Spalte,Zeile).")
			var notification = new Notification(Notification.Reinforcement)
			notification.value = player.newUnitsTemporary
			notification.currentPlayer = player
		    notifyObservers(notification);
			
		}while(player.newUnitsTemporary != 0)
	}
	
	/**
	 * Check whether the reinforcement is correct, otherwise the user get an appropriate message.
	 */
	def handleReinforcement(player:Avatar,position:Position) 
	{
	  // Spielfeld Land durch Pos bestimmen und checken ob seins
	  if(world(position.row)(position.column).checkHolder(player) && world(position.row)(position.column).getFieldType)
			{
			  world(position.row)(position.column).incArmy
			  player.newUnitsTemporary -= 1
			}
			else if(!world(position.row)(position.column).getFieldType)
			  sendNotificationInfo("Ein Wasserfeld hat kein besitzer")
			else
			  sendNotificationInfo("Das Teritorium ist nicht dein Land.")
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
	      
	      var attackCountDices = attack.getArmy
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
	       sendNotificationInfo("Wuerfel Augen: " + attackDice(i))
//	       println("Wuerfel Augen: " + attackDice(i))
	     }
	     for(i <- 0 to defenseDice.length -1)
	     {
	       defenseDice(i) = dice.roll
	       sendNotificationInfo("Wuerfel Augen: " + defenseDice(i))
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
	     
	     moveUnit(attack,defense,attackCountDices)
	     
	    }else
	    {
	      sendNotificationInfo("Ist kein Feld sondern Wasser")
	    }
	    
	  }  
	  else
	    sendNotificationInfo("Ist kein Feld sondern Wasser")
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
	  if(defense.getArmy == 0){
	    	 var in:Int = 0
	    	 sendNotificationInfo("Sieg!!")
	    	 if(attack.getArmy > 3)
	    	 {
	    	   do
	    	   {
		    	   sendNotificationInfo("Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		    	   sendNotificationInfo("Hinweis: Eine Einheit muss stationiert bleiben.")
	    	       sendNotificationInfo("Attack: Noch verbliebene Einheiten: " + attack.getArmy)
	    	       var n = new Notification(Notification.Attack)
		    	   notifyObservers(n);
	    	   }while (!attack.permissionMoveArmy)
		    	 
	    	 }
	    	 else{ 
		    	 setValueForAttackAndDefenseLand(attackCountDices)
	    	 }
	    	 
	    }
	  attack.permissionMoveArmy = false
	}
	
	/**
	 * Increase and decrease the units about the assigned amount for the attack- and defense-land.
	 * Preconditioned it is allowed.
	 */
	def setValueForAttackAndDefenseLand(army:Int)
	{
	   if(attackLand.permissionMoveArmy)
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
		 attackLand.setArmy(attackLand.getArmy - army)
		 defenseLand.setArmy(defenseLand.getArmy + army)
	}
	/**
	 * Set the new holder which is the attack land for the beaten land.
	 */
	def setNewHolderForBeatenLand = defenseLand.holder = attackLand.holder
	
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
			sendNotificationInfo("Zuviele Einheiten gewaehlt. Bitte erneut eingeben.")
			return false
		}
	    else
	    	return true
	    false
	}
	
	/**
	 * Send notifications to Inform the User.
	 * @param message. Informations for the user.
	 */
	def sendNotificationInfo(message:String)
	{
	  var notificationInfo = new Notification(Notification.Message)
	  notificationInfo.message = message
	  notifyObservers(notificationInfo)
	}
	
	/**
	 * Start the Battle Phase for the assigned player.
	 * @param player. Should be the current player.
	 */
	def startBattlePhase(player:Avatar)
	{
	  sendBattleNotification(player,true)
	  sendBattleNotification(player,false)
	  attack(player)
	}
	
	/**
	 * Set the attack or defense land in game. For an invalid position which is correlate with the player and the isOwnLand flag, the
	 * user will get a failure message.
	 * @param player. Should be the current player of the Game.
	 * @param position. World Position in the game.
	 * @param isOwnLand. For true the function will set the value for the attack land otherwise the defense land
	 */
	def setAttackOrDefenseLand(player:Avatar, position:Position, isOwnLand:Boolean)
	{
	  if(isOwnLand)
	  {
	    if(!checkOwnLandSelection(player, position))
	    {
		    sendNotificationInfo("Das ausgewaehlte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt.")
		    sendBattleNotification(player,true)
	    }else
	    {
	        player.checkPoint = position
			attackLand = world(position.row) (position.column)
	    }
	    
	  }
	  
	  if(!isOwnLand)
	  {
	    if(!checkForeignLandSelection(player, position))
	    {
	    	var notificationInfo = new Notification(Notification.Message)
			notificationInfo.message = "Das ausgewaehlte Land ist nicht das Land deines Feindes, bitte wiederhole die Eingabe korrekt."
		    notifyObservers(notificationInfo)
		    sendBattleNotification(player,false)
	    }else
	    {
	      defenseLand = world(position.row) (position.column)
	    }
	
	  }
	}
	
	/**
	 * Send Battle and information notifications.
	 * @param player. Should be the current player of the game.
	 * @param ownLand. Notifications for the own land of the player or foreign land.
	 */
	def sendBattleNotification(player:Avatar, ownLand:Boolean)
	{
		var notification = new Notification(Notification.Battle)
		var notificationInfo = new Notification(Notification.Message)
	  
		if(ownLand)
			notificationInfo.message = "Waehle das Land, von welchem du aus Angreifen willst."
		else
			notificationInfo.message = "Waehle das Land aus welches du Angreifen willst."
	      
		notification.currentPlayer = player
		notification.isOwnLand = ownLand
	  
		notifyObservers(notificationInfo)
		notifyObservers(notification);
	    
	}
	
	/**
	 * Manage the count of the attacks and communicate with the user about it.
	 * @param player. Should be the current player.
	 */
	def attack (player:Avatar)
	{
	  var ownLand = attackLand
	  var otherLand = defenseLand
	  var outloop = false
	  while(!outloop)
		  {
		    if(ownLand.getArmy > 1)
		    {
			    singleAttack(ownLand, otherLand)
			    if(otherLand.checkHolder(player))
			    {
			      println("Debug (attack): otherland holder == player " )
			      outloop = true
			    }
			    else{
			    	sendNotificationInfo(player.id + ": Noch verbliebene Einheiten: " + ownLand.getArmy)
			    	sendNotificationInfo(otherLand.getHolder + ": Noch verbliebe Einheiten: " + otherLand.getArmy)
			    	sendNotificationInfo("Weiter angreifen? (ja/nein)")
			    	val notification = new Notification(Notification.Question)
			    	notification.currentPlayer = player
			    	notifyObservers(notification)
			    	
					if(!player.myTurn)
					{
						outloop = true
					}
					else
					{
						outloop = false
					}
				}
		    }
		    else
		      outloop = true			  	
		  }
	}
	
	
	/**
	 * Validate whether the current player is owner of the land and send a notification for the information of the user.
	 * @param player. Should be the current player.
	 * @param position. World position of the land.
	 * @return true when the selected land is from the current player.
	 */
	def checkOwnLandSelection(player:Avatar,position:Position):Boolean =
	{
		var ownLand = world(position.row)(position.column)
		var notificationInfo = new Notification(Notification.Message)
		var ok = false
		if (ownLand.checkHolder(player))
		{
			notificationInfo.message = "Richtige Wahl"
			
			ok = true
					
		}else
		{
			notificationInfo.message = "Falsche Wahl"
			ok = false
		}
		notifyObservers(notificationInfo)
		ok
	}
	
	/**
	 * Validate whether the current player is not the owner of the land and send a notification for the information of the user.
	 * @param player. Should be the current player.
	 * @param position. World position of the land.
	 * @return true when the selected land is not from the current player.
	 */
	def checkForeignLandSelection(player:Avatar,position:Position):Boolean =
	{
		var otherLand = world(position.row)(position.column)
		var ownLand = world(player.checkPoint.row)(player.checkPoint.column)
		var notificationInfo = new Notification(Notification.Message)
		var ok = false
		if(ownLand.checkNeighbourhood(otherLand) && !otherLand.checkHolder(player))
		{
			notificationInfo.message = "Richtige Wahl"
			
			ok = true
					
		}else
		{
			notificationInfo.message = "Falsche Wahl"
			ok = false
		}
		notifyObservers(notificationInfo)
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
	
	
	/*
	 * Taktische Phase der Spieler hat einmal das Recht
	 * seine Einheiten von max. einem Land zu einem anderen Land,
	 * das ihn gehoert, zu verschieben.
	 * Danach ist die Runde fuer den Spieler Beendet und der 
	 * Naechste ist an der Reihe.
	 * */
	def startTacticPhase(player:Avatar)
	{
	  // Abfrage ob der Spieler Einheiten verschieben moechte
	  sendNotificationInfo("Taktische Phase!\nMoechten Sie Einheiten verschieben? (ja/nein)")
	  val notification = new Notification(Notification.Question)
	  notification.currentPlayer = player
	  notifyObservers(notification)
	  // falls ja fuehre Phase aus, ansonsten Zug beendet
	  if(player.myTurn)
	  {	  
	    sendTacticNotification(player,true)
	    sendTacticNotification(player,false)
	    setArmyToMove(player.fromLand, player.toLand, player)
	  }
	}
	
	/**
	 * Set the from or to land.
	 * Inform the user about the result of the given position.
	 * @param player. Should be the current player of the game.
	 * @param position. Position of the game world.
	 * @param from. To set the from (Source) land the parameter require the value true, otherwise false for the to (Target) land.  
	 */
	def setFromOrTo(player:Avatar, position:Position,from:Boolean)
	{

	    if(!checkOwnLandSelection(player, position))
	    {
		    sendNotificationInfo("Das ausgewaehlte Land ist nicht dein eigenes Land, bitte wiederhole die Eingabe korrekt.")
		    sendTacticNotification(player,from)
	    }else
	    {
	      if(from)
	        if(world(position.row)(position.column).getArmy > 1)
	        	player.fromLand = world(position.row)(position.column)
	        else
	        {
	          sendNotificationInfo("Zu wenig Einheiten auf diesem Land, benoetigt werden mindestens 2 Einheiten")
	          sendTacticNotification(player,true)
	        }
	          
	      else
	      {
	        player.toLand = world(position.row)(position.column) 
	        if(player.fromLand == player.toLand)
	        {
	          sendNotificationInfo("Das zweite ausgewaehlte Land darf nicht gleich dem ersten sein")
	          sendTacticNotification(player,false)
	        }
	        if(!player.fromLand.checkNeighbourhood(player.toLand))
	        {
	          sendNotificationInfo("Ist kein Nachbarland")
	          sendTacticNotification(player,false)
	        }
	      }
	    }

	}
	
	/**
	 * Send notifications to get 'from' or to 'land' of the user.
	 * @param player. Should be the current player of the game.
	 * @param from. Give true when you require the source land otherwise false for the target land.
	 */
	def sendTacticNotification(player:Avatar,from:Boolean)
	{
		var notification = new Notification(Notification.Tactic)
	  
		if(from)
		{
		  sendNotificationInfo("Bitte waehle ein eigenes Land aus um Truppen zu verschieben")
		  notification.isFirstLand = true
		}
			
		else
		{
			sendNotificationInfo("Bitte Land angeben auf das verschoben werden soll")
			notification.isFirstLand = false
		}
		notification.currentPlayer = player
	    
		notifyObservers(notification);
	    
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
		    	   sendNotificationInfo("Bitte waehle aus wieviele Einheiten du verschieben moechtest!")
		    	   sendNotificationInfo("Hinweis: Eine Einheit muss stationiert bleiben.")
	    	       sendNotificationInfo("Einheiten: " + from.getArmy)
	    	       var n = new Notification(Notification.Army)
		    	   n.currentPlayer = player
		    	   notifyObservers(n);
	    	   }while (!from.permissionMoveArmy)
		    	 
	    	     for(i <- 0 to player.newUnitsTemporary -1)
	    	     {
	    	       from.decArmy
	    	       to.incArmy
	    	     }
	    	 player.newUnitsTemporary = 0    
	    	 from.permissionMoveArmy = false
   	 
	     }
}