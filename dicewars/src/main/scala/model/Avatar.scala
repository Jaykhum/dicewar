package main.scala.model

class Avatar {
	val id = 0						// player-id/logo
	val myTurn = false				// turn?
	var occupiedTerritory = 0		// lands holden by player
	def getTerritories= occupiedTerritory
}