package main.scala.model

class Avatar(var id:Integer) {
	var myTurn = false				// turn?
	var newUnitsTemporary:Integer = 0	//amount of army to assign to a land
	var occupiedTerritory = 0		// lands holden by player
	var inputCorrect:Boolean = false
	var lost:Boolean = false
	val color:String ="" 
	def getTerritories= occupiedTerritory
}