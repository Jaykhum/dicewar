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
	
// - belegbare Länder aus der Txt-File lesen
	def startReinforcement(player: Avatar)={
		player.newUnitsTemporary = player.getTerritories/divider
		if (player.newUnitsTemporary < 3) player.newUnitsTemporary = 3;
		do{
		    sendNotificationInfo("Zahl der Verstärkung: "+ player.newUnitsTemporary)
		    sendNotificationInfo("Spieler: " + player.id + " ist dran.")
		    sendNotificationInfo("Bitte Teritorium eingeben (Spalte,Zeile).")
			var notification = new Notification(Notification.Reinforcement)
			notification.value = player.newUnitsTemporary
			notification.currentPlayer = player
		    notifyObservers(notification);
			
		}while(player.newUnitsTemporary != 0)
	}
	
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
	       sendNotificationInfo("Würfel Augen: " + attackDice(i))
//	       println("Würfel Augen: " + attackDice(i))
	     }
	     for(i <- 0 to defenseDice.length -1)
	     {
	       defenseDice(i) = dice.roll
	       sendNotificationInfo("Würfel Augen: " + defenseDice(i))
//	       println("Würfel Augen: " + defenseDice(i))
	     }
	     Sorting.quickSort(attackDice)
	     Sorting.quickSort(defenseDice)
	     attackDice = inverseArray(attackDice)
	     defenseDice = inverseArray(defenseDice)
	     
	     // Für ersten Würfel
	     if(attackDice(0) > defenseDice(0))
	       defense.decArmy
	     else
	       attack.decArmy
	     // Für zweiten Würfel
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
	
	
	def moveUnit(attack: Land, defense: Land, attackCountDices:Int)
	{  
	  if(defense.getArmy == 0){
	    	 var in:Int = 0
	    	 sendNotificationInfo("Sieg!!")
	    	 if(attack.getArmy > 3)
	    	 {
	    	   do
	    	   {
		    	   sendNotificationInfo("Bitte wähle aus wieviele Einheiten du verschieben möchtest!")
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
	
	def setValueForAttackAndDefenseLand(army:Int)
	{
	   if(attackLand.permissionMoveArmy)
      {
        setArmyForAttackAndDefenseLand(army)
        setNewHolderForBeatenLand
      }
	}
	
	def setArmyForAttackAndDefenseLand(army:Int) 
	{
		 attackLand.setArmy(attackLand.getArmy - army)
		 defenseLand.setArmy(defenseLand.getArmy + army)
	}
	
	def setNewHolderForBeatenLand = defenseLand.holder = attackLand.holder
	
	def checkNumberOfUnitMove(land:Land, army:Int):Boolean =
	{
		if(land.getArmy - army < 1)
		{
			sendNotificationInfo("Zuviele Einheiten gewählt. Bitte erneut eingeben.")
			return false
		}
	    else
	    	return true
	    false
	}
	
	
	def sendNotificationInfo(message:String)
	{
	  var notificationInfo = new Notification(Notification.Message)
	  notificationInfo.message = message
	  notifyObservers(notificationInfo)
	}
	
	def startBattlePhase(player:Avatar)
	{
	  sendBattleNotification(player,true)
	  sendBattleNotification(player,false)
	  attack(player)
	}
	
	def setAttackAndDefenseLand(player:Avatar, position:Position, isOwnLand:Boolean)
	{
	  if(isOwnLand)
	  {
	    if(!checkOwnLandSelection(player, position))
	    {
			var notificationInfo = new Notification(Notification.Message)
			notificationInfo.message = "Das ausgewählte Land ist nicht dein Land, bitte wiederhole die Eingabe korrekt."
		    notifyObservers(notificationInfo)
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
			notificationInfo.message = "Das ausgewählte Land ist nicht das Land deines Feindes, bitte wiederhole die Eingabe korrekt."
		    notifyObservers(notificationInfo)
		    sendBattleNotification(player,false)
	    }else
	    {
	      defenseLand = world(position.row) (position.column)
	    }
	
	  }
	}
	
	
	def sendBattleNotification(player:Avatar, ownLand:Boolean)
	{
		var notification = new Notification(Notification.Battle)
		var notificationInfo = new Notification(Notification.Message)
	  
		if(ownLand)
			notificationInfo.message = "Wähle das Land, von welchem du aus Angreifen willst."
		else
			notificationInfo.message = "Wähle das Land aus welches du Angreifen willst."
	      
		notification.currentPlayer = player
		notification.isOwnLand = ownLand
	  
		notifyObservers(notificationInfo)
		notifyObservers(notification);
	    
	}
	
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
			    	sendNotificationInfo(player.id + ": Noch verbliebe Einheiten: " + ownLand.getArmy)
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
	 * Validate the selection of the current player and send a notification when it was allowed.
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
	
	def tactic(player: Avatar, from: Land, to: Land) = {
	  /*
	   *  check if both fields belong to the player then move the units
	   * */
	  if(to.checkHolder(player) && from.checkHolder(player) && from.checkNeighbourhood(to))
	  {
	    var ok = false
	    var in =0
	     while (ok == false)
		    	 {
			    	 println("Bitte Anzahl zu verschiebene Einheiten angeben")
			    	 in = readLine().toInt
			    	 if(from.getArmy - in < 1)
			    	   println("Zuviele Einheiten gewählt. Bitte erneut eingeben.")
			    	 else
			    		 ok = true;
		    	 }
	    	 from.setArmy(from.getArmy - in)
	    	 to.setArmy(to.getArmy + in)
	    	 println("Ist Erfolgreich verschoben worden: " +to.getArmy)
	  }else
	  {
	    println("Nicht möglich!")
	  }
	  
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
	  sendNotificationInfo("Taktische Phase!\nMöchten Sie Einheiten verschieben? (ja/nein)")
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
	
	def setFromAndTo(player:Avatar, position:Position,firstLand:Boolean)
	{

	    if(!checkOwnLandSelection(player, position))
	    {
			var notificationInfo = new Notification(Notification.Message)
			notificationInfo.message = "Das ausgewaehlte Land ist nicht dein eigenes Land, bitte wiederhole die Eingabe korrekt."
		    notifyObservers(notificationInfo)
		    sendTacticNotification(player,firstLand)
	    }else
	    {
	      if(firstLand)
	        if(world(position.row)(position.column).getArmy > 1)
	        	player.fromLand = world(position.row)(position.column)
	        else
	        {
	          sendNotificationInfo("Zu wenig Einheiten auf diesem Land, benötigt werden mindestens 2 Einheiten")
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
	
	
	def sendTacticNotification(player:Avatar,firstLand:Boolean)
	{
		var notification = new Notification(Notification.Tactic)
	  
		if(firstLand)
		{
		  sendNotificationInfo("Bitte wähle ein eigenes Land aus um Truppen zu verschieben")
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
	

	

	
	def setArmyToMove(from: Land, to: Land, player:Avatar)
	{  
	    	   do
	    	   {
		    	   sendNotificationInfo("Bitte wähle aus wieviele Einheiten du verschieben möchtest!")
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