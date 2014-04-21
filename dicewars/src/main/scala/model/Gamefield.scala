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
	val dani = "C:\\Konstanz_Studium\\3.Semester\\Risiko\\workspace\\workspace_git\\dicewars\\Maps\\"
	val jay = "C:\\study\\workspace\\dicewar\\dicewars\\Maps\\"

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
	 * initialize the world with 18x10 fields and all fields are water-fields
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
		    println("id:1")
		  }else if( (playerFieldCount(1)+1) > (fieldCount/2) )
		  {
		    playerID = 0 
		    println("id:2")
		  }
		  
		  world(fieldContainer(i).position.row) (fieldContainer(i).position.colum).setHolder(playerID)
		  println("playerid: "+ playerID)
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
		var file = dani+ map
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
			else 
			  println("This is not your land")
	}
	
	
	def singleAttack(attack: Land, defense: Land ) ={
	  // val atkDice[3]
	  // val defDice[2]
	  /* while(attack keine Einheit oder def keine Einheit oder Angreifer bricht ab){
	    Dice.roll
	  }
	  * 
	  */
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
	       println("Würfel Augen: " + attackDice(i))
	     }
	     for(i <- 0 to defenseDice.length -1)
	     {
	       defenseDice(i) = dice.roll
	       println("Würfel Augen: " + defenseDice(i))
	     }
	     Sorting.quickSort(attackDice)
	     Sorting.quickSort(defenseDice)
	     attackDice = inverseArray(attackDice)
	     defenseDice = inverseArray(defenseDice)
	     
	     if(attackDice(0) > defenseDice(0))
	       defense.decArmy
	     else
	       attack.decArmy
	     
	     if(defenseCountDices > 1  && attackCountDices >1)
	     {
	       println("Defense: " + defenseDice.length)
	       println("Attack: " + attackDice.length)
	       println("defenseCountDices "+ defenseCountDices)
	       println("attackCountDices "+ attackCountDices)
	    	if(attackDice(1) > defenseDice(1))
	    		defense.decArmy
	    	else
	    		attack.decArmy
	     }
	     
	     if(defense.getArmy == 0){
	    	 var in:Int = 0
	    	 var ok = false
	    	 if(attack.getArmy > 3){
		    	 while (ok == false)
		    	 {
			    	 println("Sieg!! Bitte wählen sie aus wieviele Einheiten sie verschieben wollen!")
			    	 println("Hinweis: Eine Einheit muss stationiert bleiben.")
			    	 println("Attack: Noch verblieber Einheiten: " + attack.getArmy)
			    	 in = readLine().toInt
			    	 if(attack.getArmy - in < 1)
			    	   println("Zuviele Einheiten gewählt. Bitte erneut eingeben.")
			    	 else
			    		 ok = true;
		    	 }
	    	 }
	    	 else 
	    	   in = attackCountDices
	    	 attack.setArmy(attack.getArmy - in)
	    	 defense.setArmy(defense.getArmy + in)
	    	 defense.setHolder(attack.getHolder)
	     }
	     
	    }else
	    {
	      println("Ist kein Feld sondern Wasser")
	    }
	    
	  }  
	  else
	    println("nein is kein nachbar")
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
	  while(outloop == false)
		  {
		    if(ownLand.getArmy > 1){
			    singleAttack(ownLand, otherLand)
			    if(otherLand.getHolder == player.id)
			    {
			      outloop = true
			    }
			    else{
				    println(player.id + "Noch verblieber Einheiten: " + ownLand.getArmy)
				    println(otherLand.getHolder + "Noch verblieber Einheiten: " + otherLand.getArmy)
				    println("Weiter angreifen? (ja/nein)")
					var in = readLine()
					if(in.equalsIgnoreCase("nein"))
					  outloop = true
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
			player.checkPoint = position
			attackLand = world(position.row) (position.column)
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
		if(ownLand.checkNeighbourhood(otherLand))
		{
			notificationInfo.message = "Richtige Wahl"
			defenseLand = world(position.row) (position.column)
			ok = true
					
		}else
		{
			notificationInfo.message = "Falsche Wahl"
			ok = false
		}
		notifyObservers(notificationInfo)
		ok		 
	}
	
	def battlePhase(player:Avatar)=
	{
	  var outloop = false
	  while(outloop == false)
	  {
		  var ownLand:Land = null
		  var otherLand:Land = null
		  /**/
		  while(outloop == false){
			  println("Wähle das Land von dem du Angreifen willst aus.")
			  println("Zuerst Spalte eingeben")
			  var in = readLine()
			  var col: Int = in.toInt
			  println("Nun Zeile eingeben")
			  in = readLine()
			  var row: Int = in.toInt
			  ownLand = world(row)(col)
			  if (ownLand.checkHolder(player))
			  {
			    println("Richtige wahl")
			    outloop = true
			  }else
			  {
			    println("Falsche Wahl1")
			  }
			  
		  }
		  
		  outloop = false
		  /**/
		  while(outloop == false){
			  println("Wähle das Land das du Angreifen willst aus.")
			  println("Zuerst Spalte eingeben")
			  var in = readLine()
			  var col: Int = in.toInt
			  println("Nun Zeile eingeben")
			  in = readLine()
			  var row: Int = in.toInt
			  otherLand = world(row)(col)
			  if(ownLand.checkNeighbourhood(otherLand))
			    {
			    println("Richtige wahl")
			    outloop = true
			    }else
			  {
			    println("Falsche Wahl2")
			  }
		  }
		  
		  outloop = false
		  while(outloop == false)
		  {
		    if(ownLand.getArmy > 1){
			    singleAttack(ownLand, otherLand)
			    if(otherLand.getHolder == player.id)
			    {
			      outloop = true
			    }
			    else{
				    println(player.id + "Noch verblieber Einheiten: " + ownLand.getArmy)
				    println(otherLand.getHolder + "Noch verblieber Einheiten: " + otherLand.getArmy)
				    println("Weiter angreifen? (ja/nein)")
					var in = readLine()
					if(in.equalsIgnoreCase("nein"))
					  outloop = true
				}
		    }
		    else
		      outloop = true
			  	
		  }
		println("Battlephase beenden? (ja/nein)")
		
		var in = readLine()
		outloop = false
		if(in.equalsIgnoreCase("ja"))
			outloop = true
			println("outloop: " + outloop )
	  }
	 
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

	
}