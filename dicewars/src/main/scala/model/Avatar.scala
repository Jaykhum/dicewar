package main.scala.model
import main.scala.util.Position

class Avatar(var id:Integer) {
	var myTurn = false				// turn?
	var newUnitsTemporary:Integer = 0	//amount of army to assign to a land
	var checkPoint:Position = null //selected ownland to be able to attack foreignland
	var occupiedTerritory = 0		// lands holden by player
	var fromLand:Land = null
	var toLand:Land = null
	def getTerritories= occupiedTerritory
}