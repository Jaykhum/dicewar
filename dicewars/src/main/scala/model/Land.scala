package main.scala.model

import main.scala.util.Notification
import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

class WorldPosition(y: Int, x: Int){
  val row: Int = y
  val column: Int = x
  
  def isSame(position: WorldPosition): Boolean= 
  {
    if(position.row ==  this.row && position.column == this.column)
      return true
    else
      return false
  }

  /*
   * Check if two fields are neighbours
   * */
  	def isBorder(position: WorldPosition):Boolean=
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
 * Dummy class for init.
 * */
abstract class Land{
val position:WorldPosition
var holder: Int 
var permissionMoveArmy:Boolean = false
var permissionSelectLand:Boolean = false
def showImage = "00"
def checkNeighbourhood(field1:Land): Boolean
def checkHolder(player: Avatar):Boolean
def incArmy
def decArmy
def getFieldType:Boolean
def setHolder(id:Int)
def getHolder:Int
def getArmy: Int
def setArmy(armyCount:Int)
}

/*
 * 
 * */
class Field(row: Int, col: Int) extends Land 
{
// number of units on the field
	var army = 4
// player-id who holds the field
	var holder: Int = -3
	
	// position in world
	val position = new WorldPosition(row, col)

	def getFieldType = true
/**/
	def getArmy = army
/**/
	def getHolder:Int = holder
/**/
	def setArmy(armyCount:Int) = this.army = armyCount
	
/**/
	def incArmy = this.army += 1

/**/
	def decArmy = this.army -= 1
	
/**/
	def setHolder(id:Int) = this.holder = id

/**/
	override def showImage = 
	 { 
	  if (army < 10)
		 "0" + army.toString
	   else
	     army.toString
	 }


/*
 * compute if the current player holds this field
 * */
	def checkHolder(player: Avatar):Boolean = {
	  if (player.id == this.holder)
	    return true
	  else
	    return false
	}

/*
 * compute if a other land is a bordering land of this ones
 * */
	def checkNeighbourhood(field1:Land): Boolean =
	{
	  if( position.isBorder(field1.position))
		  return true 
	  else
		  return false
	}
	
	
	/**
	 * Check if the player has to the declared position a neighbour which belongs not him.
	 * @param world. The world of the game.
	 * @return return true if the position has a neighbour which belongs another player.
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
	  neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))

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
	  }else
	    hasEnemyNeighbour = false     
	    
	    hasEnemyNeighbour
	}
	
	/**
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @param world. The world of the game.
	 * @return return true if the position has a neighbour which belongs player.
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
	  }else
	    hasOwnNeighbour = false     
	    
	    hasOwnNeighbour
	}
	
	def getOwnNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
	{
	  var ownLand = this
	  
	  var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
	  var neighbourContainer = new ArrayBuffer[Field]()
	  
	  neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
	  neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
	  neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
	  neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
	  
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
	
	
	def getEnemyNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
	{
	  var ownLand = this
	  
	  var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
	  var neighbourContainer = new ArrayBuffer[Field]()
	  
	  neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
	  neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
	  neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
	  neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
	  
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
	
	
	
	/**
	 * Gives the count of enemy neighbours.
	 * @param world. The world of the game.
	 * @return return the count of enemy Neigbours.
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
	

	/**
	 * Check if the player has to the declared position a neighbour which belongs to him.
	 * @param world. The world of the game.
	 * @return return true if the position has a neighbour which belongs another player.
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
	  }else
	    hasWeakerEnemyNeighbour = false     
	    
	    hasWeakerEnemyNeighbour
	}

	/**
	 * Give the weakest neighbour as measured by the amound of army.
	 * Are there more than one weakest neighbour because of equal of the army, 
	 * the function will just return one of it per clockwise rotation => 1.top- 2.right- 3.down 4.left-land.
	 * @param world. The world of the game.
	 * @return return null if there is no land or the neighbour land is stronger as the ownland otherwise it will return the land.
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
	  neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
	  for(i <- 0 until neighbourContainer.length)

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
	  }else
	    weakestNeighbour = null   
	    
	    weakestNeighbour
	}
}


class Water(row: Int, col: Int) extends Land 
{
  // position in world
  val position = new WorldPosition(row, col)
  val sign = "ww"
  var holder: Int = -1
  override def showImage = sign
  def setArmy(armyCount:Int) = println("this is a water field")
  def getArmy = 0
  def setHolder(id:Int) = println("this is a water field")
  def getHolder:Int = holder
  def getFieldType = false
 /**/
	def incArmy() = println("this is a water field")
 /**/
	def decArmy() = println("this is a water field")	
 /*
 * compute if the current player holds this field
 * */
	def checkHolder(player: Avatar):Boolean = {
	    return false
	}
 
 /*
 * compute if a other land is a bordering land of this ones
 * */
  	def checkNeighbourhood(field1:Land): Boolean =
	{
	  if( position.isBorder(field1.position))
		  return true 
	  else
		  return false
	}
}