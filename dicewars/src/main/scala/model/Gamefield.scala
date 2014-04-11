package main.scala.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.StringOps
import main.scala.util.FileUtil
import scala.util.Random

class Gamefield {
	val divider = 3
	val height = 10;
	val width = 18;
	val playercount = 2;
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](height,width)
	
	/*
	 * initialize the world with 18x10 fields and all fields are water-fields
	 * */
	def initMap
	{
	  for(i <- 0 to height-1; j <- 0 to width-1)
	  {
	    world(i)(j) = new Water(i,j)
	  }
	}
	
	/*
	 * read the map form a txt-file and defines which fields are lands or water
	 * */
	def mapPosition(map:String)
	{
		val fu = new FileUtil
		var file = "C:\\study\\workspace\\dicewar\\dicewars\\Maps\\"+ map
		var outArray = fu.readData(file)
		var Position = new Array[Int](2)
		println(playercount)
	  	val rnd = new Random(playercount)
		var playerID =  0
		var playerFieldCount = new Array[Int](2)
		var fieldCount:Int = outArray.size 
		playerFieldCount(0) = 0
		playerFieldCount(1) = 0
		for(a <- 0 to outArray.size-1){
		  
		  var s = outArray.apply(a)
		  var charArray = s.toCharArray
		  // row 
		  Position(0) = String.valueOf(charArray(0)).toInt
		  // colum

		  if(charArray.length == 3)	Position(1) = String.valueOf(charArray(2)).toInt
		  else if(charArray.length == 4)	Position(1) = String.valueOf(charArray(3)).toInt + 10
		  world(Position(0)) (Position(1)) = new Field(Position(0), Position(1))
		  
		  playerID =  rnd.nextInt(playercount)
		  println("playerid: "+ playerID)
		  if( (playerFieldCount(0)+1) > (fieldCount/2) )
		    playerID = 2
		  else if( (playerFieldCount(1)+1) > (fieldCount/2) )
		    playerID = 1 
		  
		  world(Position(0)) (Position(1)).setHolder(playerID+1)
		  playerFieldCount(playerID) += 1
		}
	}
	
	def getWorld() = world
	
// - belegbare Länder aus der Txt-File lesen
	def reinforcement(player: Avatar)={
		var amountNewUnits = player.getTerritories/divider
		if (amountNewUnits < 3) amountNewUnits = 3;
		do{
			println("Amount of Reinforcement: "+ amountNewUnits)
			println("Please insert territory (row,colum).")
			println("First insert the colum of the field")
			var in = readLine()
			var col: Int = in.toInt
			println("Now insert the row of the field")
			in = readLine()
			var row: Int = in.toInt

			// Spielfeld Land durch Pos bestimmen und checken ob seins
			if(world(row)(col).checkHolder(player) && world(row)(col).getFieldType)
			{
			  world(row)(col).addArmy
			  amountNewUnits -= 1
			}
			else 
			  println("this is not your land")
		}while(amountNewUnits != 0)
		  
	}
	
	def battle(attack: Land, defense: Land )={
	  // val atkDice[3]
	  // val defDice[2]
	  /* while(attack keine Einheit oder def keine Einheit oder Angreifer bricht ab){
	    Dice.roll
	  }
	  * 
	  */
	  if(attack.checkNeighbourhood(defense))
	    println("ja is nachbar")
	  else
	    println("nein is kein nachbar")
	}
	
	
	def tactic(player: Avatar, from: Land, to: Land) = {
	  /*
	   *  check if both fields belong to the player then move the units
	   * */
	}

	
}