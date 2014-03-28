package main.scala.model

class Gamefield {
	val divider = 3
// TODO:
// - Spielfeld 20x10, ein Land braucht 4x3 (breite/höhe) Zeichen
	val world = Array.ofDim[Land](10,20)
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