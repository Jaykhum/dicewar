package main.scala.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.StringOps
import main.scala.util.FileUtil

class Gamefield {
	val divider = 3
// TODO:
	val height = 10;
	val width = 18;
	// - Spielfeld 20x10, ein Land braucht 4x3 (breite/höhe) Zeichen
	val world = Array.ofDim[Land](height,width)
	
	def initMap
	{

	  for(i <- 0 to height-1; j <- 0 to width-1)
	  {
	    world(i)(j) = new Water
	  }
	}
	
	def mapPosition(map:String)
	{
		val fu = new FileUtil
		var file = "C:\\Konstanz_Studium\\3.Semester\\Risiko\\workspace\\workspace_git\\dicewars\\Maps\\"+ map
		var outArray = fu.readData(file)
		var Position = new Array[Int](2)
	  	var a = 0
		for(a <- 0 to outArray.size-1){
		  
		  var s = outArray.apply(a)
		  var charArray = s.toCharArray
		  // row 
		  Position(0) = String.valueOf(charArray(0)).toInt
		  // colum

		  if(charArray.length == 3)	Position(1) = String.valueOf(charArray(2)).toInt
		  else if(charArray.length == 4)	Position(1) = String.valueOf(charArray(3)).toInt + 10
		  world(Position(0)) (Position(1)) = new Field
		  
		}
	}
	
	def getWorld() = world
	
// - belegbare Länder aus der Txt-File lesen
	def reinforcement(player: Avatar)={
		var amountNewUnits = player.getTerritories/divider
		if (amountNewUnits < 3) amountNewUnits = 3;
		do{
			println("Please insert territory (row,colum)")
			var row = readLine()
			var col = readLine()
			// Spielfeld Land durch Pos bestimmen und checken ob seins
			amountNewUnits -= 1
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
	}
	
	
	def tactic(player: Avatar, from: Land, to: Land) = {
	  /*
	   *  check if both fields belong to the player then move the units
	   * */
	}
  
}