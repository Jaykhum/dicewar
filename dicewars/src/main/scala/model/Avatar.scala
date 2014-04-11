package main.scala.model

class Avatar {
	var id = 0						// player-id/logo
	val myTurn = false				// turn?
	var occupiedTerritory = 0		// lands holden by player
	def getTerritories= occupiedTerritory
	def setId(id:Int) =  this.id = id
}