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
	// Gameboard with size 18x10 fields and each fields needs 4x3 (width/height) Signs
	val world = Array.ofDim[Land](height,width)
	val dani = "C:\\Konstanz_Studium\\3.Semester\\Risiko\\workspace\\workspace_git\\dicewars\\Maps\\"
	val jay = "C:\\study\\workspace\\dicewar\\dicewars\\Maps\\"
	
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
		var file = dani+ map
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
			  world(row)(col).incArmy
			  amountNewUnits -= 1
			}
			else 
			  println("this is not your land")
		}while(amountNewUnits != 0)
		  
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