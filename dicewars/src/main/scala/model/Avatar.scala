package main.scala.model

/*
 * Companian class for Avatar object
 * */
class Avatar(val id:Integer) {
	// answer response from player 
	var answer: Boolean = false				
	// amount of army to assign to a land (reinforcement phase) 
	var newUnitsTemporary:Integer = 0	
	// lands holden by player
	var occupiedTerritory = 0
	// input flag for user input
	var inputCorrect:Boolean = false
	// playerflag true means this player was killed 
	var lost:Boolean = false
	// message color for display the playerinfotext 
	var color:Avatar.ColorTyp =null
	// min. amount of unit the user must move accorind to the dice amount
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