package main.scala.model

class Avatar(val id:Integer) {
	var answer: Boolean = false				// answer response from player 
	var newUnitsTemporary:Integer = 0	//amount of army to assign to a land
	var occupiedTerritory = 0		// lands holden by player
	var inputCorrect:Boolean = false
	var lost:Boolean = false
	var color:Avatar.ColorTyp =null
	//var inputType:String = ""
	var minimumMove:Int = 0
	def getTerritories= occupiedTerritory
}

object Avatar extends Enumeration 
{
    type ColorTyp = Value
    val divider = 3
    val Blue, Mangenta, Green  = Value
    val colorContainer = Array[ColorTyp](Blue,Mangenta,Green)
}