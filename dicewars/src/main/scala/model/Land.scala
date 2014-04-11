package main.scala.model

import java.lang.Boolean

class WorldPosition(y: Int, x: Int){
  val row: Int = y
  val colum: Int = x
  
  def isSame(position: WorldPosition): Boolean = 
  {
    if(position.row ==  this.row && position.colum == this.colum)
      return true
    else
      return false
  }
  
//	  def isBorder(position: WorldPosition): Boolean =
//	  {
//	    val rightN = new WorldPosition(this.row, this.colum + 1)
//	    val leftN = new WorldPosition(this.row, this.colum - 1)
//	    val upperN = new WorldPosition(this.row + 1, this.colum)
//	    val bottomN = new WorldPosition(this.row - 1, this.colum)
//	    
//	    position match{ 
//	      case rightN => return true
//	      case leftN => return true
//	      case upperN => return true
//	      case bottomN => return true
//	      case _ => return false
//	      }
//	  }
  /*
   * Check if two fields are neighbours
   * */
  	def isBorder(position: WorldPosition): Boolean =
  	{
  	 	if(((position.row == this.row - 1) && (position.colum == this.colum)) || ((position.row == this.row + 1) && (position.colum == this.colum)))
  	 	  return true
  	 	else if(((position.colum == this.colum - 1) && (position.row == this.row)) || ((position.colum == this.colum + 1) && (position.row == this.row)))
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
def showImage = "00"
def checkNeighbourhood(field1:Land): Boolean
def checkHolder(player: Avatar):Boolean
def addArmy
def getFieldType:Boolean
def setHolder(id:Int)
}

/*
 * 
 * */
class Field(row: Int, col: Int) extends Land 
{
// number of units on the field
	var army = 1
// player-id who holds the field
	var holder: Int = 0
// position in world
	val position = new WorldPosition(row, col)

	def getFieldType = true
/**/
	def getArmy = army
/**/
	def getHolder = holder
/**/
	def setArmy(armyCount:Int) = this.army = armyCount
	
/**/
	def addArmy() = this.army += 1
	
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

}

class Water(row: Int, col: Int) extends Land 
{
  // position in world
  val position = new WorldPosition(row, col)
  val sign = "ww"
  var holder: Int = 0
  override def showImage = sign
  
  def setHolder(id:Int) = holder = -1
  def getFieldType = false
 /**/
	def addArmy() = println("this is a water field")
	
 /*
 * compute if the current player holds this field
 * */
	def checkHolder(player: Avatar):Boolean = {
	  	println("A water-field has no holder")
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