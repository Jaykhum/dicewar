package main.scala.model

// scala packages 
import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

// own costum packages
import main.scala.util.Notification


/*
 * defines a 2D-position in the world of the game
 * @ y: the row position in the world grid
 * @ x: the column position in the world grid
 * */
class WorldPosition(y:Int, x:Int){
	
	val column: Int = x		// column value of this field
	val row: Int = y		// row value of this field
	
	
	/*
	 * compute if the given field position is the same as this on
	 * if yes return true, else false
	 * @ position: the 2d position of the other territory
	 * */
	def isSame(position:WorldPosition) : Boolean= 
	{
		if(position.row ==  this.row && position.column == this.column)
	      return true
	    else
	      return false
	}
	
	
	/*
	 * check if the two given territories are neighbours
	 * @ position: the 2d position of the other territory
	 * */
  	def isBorder(position:WorldPosition) : Boolean=
  	{
  	 	if(((position.row == this.row - 1) && (position.column == this.column)) || ((position.row == this.row + 1) && (position.column == this.column)))
  	 	  return true
  	 	else if(((position.column == this.column - 1) && (position.row == this.row)) || ((position.column == this.column + 1) && (position.row == this.row)))
  	 	  return true
  	 	else
  	 	  return false
  	}
}




/*
 * A abstract class for initialization
 * */
abstract class Land
{
	val position:WorldPosition
	var holder: Int 
	var permissionMoveArmy:Boolean = false
	var permissionSelectLand:Boolean = false
	def showImage: String = "00"
	def checkNeighbourhood(field1:Land) : Boolean
	def checkHolder(player:Avatar) : Boolean
	def incArmy
	def decArmy
	def getFieldType : Boolean
	def setHolder(id:Int)
	def getHolder : Int
	def getArmy : Int
	def setArmy(armyCount:Int)
}




/*
 * defines a land/continental type field
 * parent class is Land
 * @ row: the row position in the world grid
 * @ col: the column position in the world grid
 * */
class Field(row:Int, col:Int) extends Land 
{
	// amount of units which are positioned on this field
	var army = 4
	// player-id who holds this field
	var holder: Int = -3
	
	// position in world
	val position = new WorldPosition(row, col)
		
	
	/*
	 * check if the player has a territory as a neighbour which belongs to an enemy.
	 * return true if this land has a neighbour which belongs an another player.
	 * @ world: contains the hole world of this game
	 */
	def checkHasEnemyNeighbour(world: Array[Array[Land]]):Boolean =
	{
		var ownLand = this
		var neighbourContainer = new ArrayBuffer[WorldPosition]()
		  
		neighbourContainer += new WorldPosition(position.row, position.column-1)
		neighbourContainer += new WorldPosition(position.row, position.column+1)
		neighbourContainer += new WorldPosition(position.row+1, position.column)
		neighbourContainer += new WorldPosition(position.row-1, position.column)
		  
		var hasEnemyNeighbour = false
		// filtering, pick only the fields which are inside the world bounderies
		neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
		// if just one field is a land-type field returns ture 
		if(neighbourContainer.length != 0)
		{
			for(i <- 0 until neighbourContainer.length)
		    {
				var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
				if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType)
				{
					hasEnemyNeighbour = true
				}
		    }
		}
		else	hasEnemyNeighbour = false     
		hasEnemyNeighbour
	}
	
	/*
	 * check if the player has a territory as a neighbour which belongs to himself
	 * return true if the position has a neighbour
	 * @ world: contains the hole world of this game
	 */
	def checkHasOwnNeighbour(world: Array[Array[Land]]):Boolean =
	{
		var ownLand = this
		var neighbourContainer = new ArrayBuffer[WorldPosition]()
		
		neighbourContainer += new WorldPosition(position.row, position.column-1)
		neighbourContainer += new WorldPosition(position.row, position.column+1)
		neighbourContainer += new WorldPosition(position.row+1, position.column)
		neighbourContainer += new WorldPosition(position.row-1, position.column)
	  
		var hasOwnNeighbour = false
		// filtering, pick only the fields which are inside the world bounderies
		neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
		if(neighbourContainer.length != 0)
		{
			for(i <- 0 until neighbourContainer.length)
			{
				var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
				if(neighbourLand.getHolder == ownLand.getHolder && neighbourLand.getFieldType)
				{
					hasOwnNeighbour = true
				}
			}
		}
		else	hasOwnNeighbour = false
	    hasOwnNeighbour
	}
		

	/*
	 * check if the player has a territory as a neighbour which belongs to an enemy and holding lesser units
	 * @ world: contains the hole world of this game
	 */
	def checkHasWeakerEnemyNeighbour(world: Array[Array[Land]]):Boolean =
	{
		var ownLand = this
		var neighbourContainer = new ArrayBuffer[WorldPosition]()
		
		neighbourContainer += new WorldPosition(position.row, position.column-1)
		neighbourContainer += new WorldPosition(position.row, position.column+1)
		neighbourContainer += new WorldPosition(position.row+1, position.column)
		neighbourContainer += new WorldPosition(position.row-1, position.column)
	  
		var hasWeakerEnemyNeighbour = false
		// filtering, pick only the fields which are inside the world bounderies
		neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))

		if(neighbourContainer.length != 0)
		{
			for(i <- 0 until neighbourContainer.length)
			{
				var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
				if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType && 
					neighbourLand.getArmy < ownLand.getArmy)
				{
					hasWeakerEnemyNeighbour = true
				}
			}
		}
		else	hasWeakerEnemyNeighbour = false
	    hasWeakerEnemyNeighbour
	}
	

	/*
	 * compute if the current player holds this field
	 * if the id of the land and the player are the same
	 * return true, else false
	 * @ player: contains information about the player
	 * */
	def checkHolder(player:Avatar) : Boolean = {
		if (player.id == this.holder)
			return true
		else
		    return false
	}
	

	/*
	 * compute if a other land is a neighbourland of this one
	 * returns true if it's a left or right or top or bottom neighbour
	 * else false
	 * @ field1: contains the other territory 
	 * */
	def checkNeighbourhood(field1:Land): Boolean =
	{
		if( position.isBorder(field1.position))
			return true 
		else
			return false
	}
	
	
	/*
	 * decreases the unit amount by 1
	 * */
	def decArmy = this.army -= 1
	
	
	/*
	 * increases the unit amount by 1
	 * */
	def incArmy = this.army += 1
	
	
	/*
	 * returns the amount of units on this field
	 * */
	def getArmy:Int = army
		
	
	/*
	 * compute the all enemy neighbour territories.
	 * return an array with all founded enemy Neigbours territories
	 * @ world: contains the hole world of this game.
	 */
	def getEnemyNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
	{
		var ownLand = this
		var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
		var neighbourContainer = new ArrayBuffer[Field]()
	  
		neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
		neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
		neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
		neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
		
		// filtering, pick only the fields which are inside the world bounderies
		neighbourContainerPosition = neighbourContainerPosition.filter(pos => World.checkPositionIsInWorld(pos))
	  
		if(neighbourContainerPosition.length != 0)
		{
			for(i <- 0 until neighbourContainerPosition.length)
			{
				var neighbourLand = world(neighbourContainerPosition.apply(i).row)(neighbourContainerPosition.apply(i).column)
				if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType)
				{
					neighbourContainer += neighbourLand.asInstanceOf[Field]
				}
			}
		}
	    neighbourContainer
	}
		
	
	/*
	 * this is land type field, so return true
	 * */
	def getFieldType:Boolean = true

	
	/*
	 * returns the id of the field holder
	 * */
	def getHolder:Int = holder
	
	
	/*
	 * compute the amount of enemy neighbour territories.
	 * return the count of enemy Neigbours
	 * @ world: contains the hole world of this game.
	 */
	def getNumberOfEnemyNeighbour(world: Array[Array[Land]]):Int =
	{
	    var ownLand = this
	    var enemyNumber = 0;
		var neighbourContainer = new ArrayBuffer[WorldPosition]()
	  
		neighbourContainer += new WorldPosition(position.row, position.column-1)
		neighbourContainer += new WorldPosition(position.row, position.column+1)
		neighbourContainer += new WorldPosition(position.row+1, position.column)
		neighbourContainer += new WorldPosition(position.row-1, position.column)
	  
		for(i <- 0 until neighbourContainer.length)
		{
			if(World.checkPositionIsInWorld(neighbourContainer.apply(i)))
			{
				var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
				if(neighbourLand.getFieldType && neighbourLand.getHolder != ownLand.getHolder)
					enemyNumber += 1
			}
		}
		enemyNumber
	}
	
	
	/*
	 * compute the all neighbour territories which belongs the current player
	 * return an array with all founded neigbours territories
	 * @ world: contains the hole world of this game.
	 */
	def getOwnNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
	{
	    var ownLand = this
	    var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
	    var neighbourContainer = new ArrayBuffer[Field]()
	    neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
	    neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
	    neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
	    neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
	  
	    // filtering, pick only the fields which are inside the world bounderies
	    neighbourContainerPosition = neighbourContainerPosition.filter(pos => World.checkPositionIsInWorld(pos))

	    if(neighbourContainerPosition.length != 0)
	    {
	    	for(i <- 0 until neighbourContainerPosition.length)
	    	{
	    		var neighbourLand = world(neighbourContainerPosition.apply(i).row)(neighbourContainerPosition.apply(i).column)
	    		if(neighbourLand.getHolder == ownLand.getHolder && neighbourLand.getFieldType)
	    		{
	    			neighbourContainer += neighbourLand.asInstanceOf[Field]
	    		}
	    	}
	    }	    
	    neighbourContainer
	}


	/*
	 * compute the weakest neighbour as measured by the amount of the positioned units
	 * are there more than one weakest neighbour because of equal of the army, 
	 * the function will just return one of it per clockwise rotation => 1.top- 2.right- 3.down 4.left-land.
	 * return null if there is no land or all neighbour territories are stronger as this, otherwise return the land
	 * @ world: contains the hole world of this game.
	 */
	def getWeakestEnemyNeighbour(world: Array[Array[Land]]):Land =
	{
	    var ownLand = this
	    var neighbourContainer = new ArrayBuffer[WorldPosition]()
	    
	    neighbourContainer += new WorldPosition(position.row+1, position.column)
	    neighbourContainer += new WorldPosition(position.row, position.column+1)
	    neighbourContainer += new WorldPosition(position.row-1, position.column)
	    neighbourContainer += new WorldPosition(position.row, position.column-1)  
	  	  
	    var weakestNeighbour:Land = null
	    // filtering, pick only the fields which are inside the world bounderies
	    neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
	   	if(neighbourContainer.length != 0)
	    {
	    	var weakestArmy = ownLand.getArmy
	    	for(i <- 0 until neighbourContainer.length)
	    	{
	    		var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
	    		if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType && 
	    		neighbourLand.getArmy < ownLand.getArmy && neighbourLand.getArmy < weakestArmy)
	    		{
	    			weakestArmy = neighbourLand.getArmy
	    			weakestNeighbour = neighbourLand
	    		}
	    	 }
	    }
	    else	weakestNeighbour = null   
	    weakestNeighbour
	}
	
	
	/*
	 * changes the amount of the units on this land
	 * @ armyCount: contains the new amount
	 * */
	def setArmy(armyCount:Int) = this.army = armyCount
	

	/*
	 * changes the id of the field holder
	 * @ id: new holders id
	 * */
	def setHolder(id:Int) = this.holder = id

	
	/*
	 * returns a string with to digits as a logo
	 * */
	override def showImage:String = 
	{ 
		// if the unit count is under 10 set a 0 as the first digit
		if (army < 10)
			"0" + army.toString
		else
			army.toString
	}
}




/*
 * defines a water field
 * parent class is Land
 * @ row: the row position in the world grid
 * @ col: the column position in the world grid
 * */
class Water(row: Int, col: Int) extends Land 
{
	// position in world
    var holder: Int = -1						// holder id
	val position = new WorldPosition(row, col)	// the position of this field on the world
  	val sign = "ww"								// logo of this field
  
  	
  	/*
  	 * returns the sign which represents this fiel type on the TUI
  	 * */
  	override def showImage = sign


  	/*
	 * compute if the current player holds this field
	 * @ player: contains information about the current player
	 * */
	def checkHolder(player: Avatar):Boolean = return false
 
  
	/*
	 * compute if a other land is a bordering land of this ones
	 * @ field1: contains a second territory
	 * */
	def checkNeighbourhood(field1:Land): Boolean =
	{
	  	if( position.isBorder(field1.position))
	  		return true 
		else
			return false
	}
   
	
	/*
	 * This is a dummy function.
	 * A water field cannot hold any units
	 * */
	def decArmy() = println("this is a water field")
	
	
	/*
	 * returns the amount of the units on this field
	 * */
	def getArmy = 0

	
	/*
	 * returns the id of the holder on this field
	 * */
	def getHolder:Int = holder
	
	
	/*
	 * returns that this field is not a land field type
	 * */
	def getFieldType = false
	  	
  	
  	/*
	 * This is a dummy function.
	 * A water field cannot hold any units
	 * */
	def incArmy() = println("this is a water field")
	
	
  	/*
	 * This is a dummy function.
	 * A water field cannot hold any units
	 * @ id: contains the id of the field holder 
	 * */
  	def setHolder(id:Int) = println("this is a water field")
  	
  	  
  	/*
	 * This is a dummy function.
	 * A water field cannot hold any units
	 * @ armyCount: contains the amount of units the users like to put on this field
	 * */
  	def setArmy(armyCount:Int) = println("this is a water field")
}