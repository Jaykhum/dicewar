package test.scala.model

import org.specs2.mutable._
import main.scala.model.WorldPosition
import main.scala.model.Land
import main.scala.model.Field
import main.scala.model.Avatar
import main.scala.model.Water

class LandSpec extends Specification
{
	"A WorldPosition" should
	{	  
	  val position1 = new WorldPosition(3,2)
	  val position2 = new WorldPosition(2,3)
	  val position3 = new WorldPosition(4,2)
	  
	  "have an row value" in
	  {
	    position1.row must be_==(3)
	  }
	  
	  "have an column value" in
	  {
	    position1.column must be_==(2)
	  }
	  
	  "be the same as the other" in
	  {
	    position1.isSame(position1) must beTrue
	  }
	  
	  "be not the same as the other" in
	  {
	    position1.isSame(position2) must beFalse
	  }
	  
	  "be a neighbour from the other" in
	  {
	    position1.isBorder(position3) must beTrue
	  }
	  
	  "be not a neighbour from the other" in
	  {
	    position1.isBorder(position2) must beFalse
	  }
	}
	
	
	"A Field" should
	{
		val field1 = new Field(12, 8)
		val field2 = new Field(12, 9)
		val field3 = new Field(4, 5)
		val size = 10
		
		"have an army value" in
		{
			field1.army must be_==(4)
		}
		
		"have an holder id value" in
		{
			field1.holder must be_==(-3)
		}
		
		"have an position value" in
		{
			field1.position.row  must be_==(12)
			field1.position.column must be_==(8)
		}
		
		"be able to return fieldtype = true" in
		{
			field1.getFieldType must beTrue
		}
		
		"be able to return holder id" in
		{
			field1.getHolder must be_==(-3)
		}
		
		"be able to return Unit amount on field" in
		{
			field1.army must be_==(4)
		}
		
		"be able to set Unit amount on field" in
		{
			var testField =  new Field(1, 1)
			testField.setArmy(60) 
			testField.getArmy must be_==(60)
		}
		
		"be able to increase Unit amount on field" in
		{
			var testField =  new Field(1, 1)
			testField.setArmy(60)
			testField.incArmy
			testField.getArmy must be_==(61)
		}
		
		"be able to increase Unit amount on field" in
		{
			var testField =  new Field(1, 1)
			testField.setArmy(60)
			testField.decArmy
			testField.getArmy must be_==(59)
		}
		
		"be able to increase Unit amount on field" in
		{
			var testField =  new Field(1, 1)
			testField.setHolder(9)
			testField.getHolder must be_==(9)
		}
		
		"be able to check holder is ok" in
		{
			var testField =  new Field(1, 1)
			var testPlayer = new Avatar(9)
			testField.setHolder(9)
			testField.checkHolder(testPlayer) must beTrue
		}
		
		"be able to check holder is wrong" in
		{
			var testField =  new Field(1, 1)
			var testPlayer = new Avatar(3)
			testField.setHolder(9)
			testField.checkHolder(testPlayer) must beFalse
		}
		
		"be able to check neighbour is correct" in
		{
			field1.checkNeighbourhood(field2) must beTrue
		}
		
		"be able to check neighbour is wrong" in
		{
			field1.checkNeighbourhood(field3) must beFalse
		}
		
		"be able to check has enemy neighbour is correct" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			var testField =  new Field(4, 3)
			testField.setHolder(9)
			testField.checkHasEnemyNeighbour(testWorld) must beTrue
		}
		
		"be able to check has enemy neighbour is wrong" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			var testField =  new Field(1, 3)
			testField.setHolder(9)
			testField.checkHasEnemyNeighbour(testWorld) must beFalse
		}
		
		"be able to check has own neighbour is correct" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			testField.checkHasOwnNeighbour(testWorld) must beTrue
		}
		
		"be able to check has enemy neighbour is wrong" in
		{
			
			val testWorld = Array.ofDim[Land](size,size)
		    for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			var testField =  new Field(4, 3)
			testField.setHolder(9)
			testField.checkHasOwnNeighbour(testWorld) must beFalse
		}
	}
}

//class Field(row: Int, col: Int) extends Land 
//{
//	
//	def getOwnNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
//	{
//	  var ownLand = this
//	  
//	  var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
//	  var neighbourContainer = new ArrayBuffer[Field]()
//	  
//	  neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
//	  neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
//	  neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
//	  neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
//	  
//	  neighbourContainerPosition = neighbourContainerPosition.filter(pos => World.checkPositionIsInWorld(pos))
//
//	  if(neighbourContainerPosition.length != 0)
//	  {
//	    for(i <- 0 until neighbourContainerPosition.length)
//	    {
//	      var neighbourLand = world(neighbourContainerPosition.apply(i).row)(neighbourContainerPosition.apply(i).column)
//	      if(neighbourLand.getHolder == ownLand.getHolder && neighbourLand.getFieldType)
//	      {
//	        neighbourContainer += neighbourLand.asInstanceOf[Field]
//	      }
//	    }
//	  }   
//	    
//	    neighbourContainer
//	}
//	
//	
//	def getEnemyNeighbourContainer(world: Array[Array[Land]]):ArrayBuffer[Field]=
//	{
//	  var ownLand = this
//	  
//	  var neighbourContainerPosition = new ArrayBuffer[WorldPosition]()
//	  var neighbourContainer = new ArrayBuffer[Field]()
//	  
//	  neighbourContainerPosition += new WorldPosition(position.row, position.column-1)
//	  neighbourContainerPosition += new WorldPosition(position.row, position.column+1)
//	  neighbourContainerPosition += new WorldPosition(position.row+1, position.column)
//	  neighbourContainerPosition += new WorldPosition(position.row-1, position.column)
//	  
//	  neighbourContainerPosition = neighbourContainerPosition.filter(pos => World.checkPositionIsInWorld(pos))
//	  
//	  if(neighbourContainerPosition.length != 0)
//	  {
//	    for(i <- 0 until neighbourContainerPosition.length)
//	    {
//	      var neighbourLand = world(neighbourContainerPosition.apply(i).row)(neighbourContainerPosition.apply(i).column)
//	      if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType)
//	      {
//	        neighbourContainer += neighbourLand.asInstanceOf[Field]
//	      }
//	    }
//	  }
//	    neighbourContainer
//	}
//	
//	
//	
//	/**
//	 * Gives the count of enemy neighbours.
//	 * @param world. The world of the game.
//	 * @return return the count of enemy Neigbours.
//	 */
//	def getNumberOfEnemyNeighbour(world: Array[Array[Land]]):Int =
//	{
//	  var ownLand = this
//	  var enemyNumber = 0;
//	  var neighbourContainer = new ArrayBuffer[WorldPosition]()
//	  
//	  neighbourContainer += new WorldPosition(position.row, position.column-1)
//	  neighbourContainer += new WorldPosition(position.row, position.column+1)
//	  neighbourContainer += new WorldPosition(position.row+1, position.column)
//	  neighbourContainer += new WorldPosition(position.row-1, position.column)
//	  
//	  
//	  for(i <- 0 until neighbourContainer.length)
//	  {
//	    if(World.checkPositionIsInWorld(neighbourContainer.apply(i)))
//	    {
//	      var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
//	      if(neighbourLand.getFieldType && neighbourLand.getHolder != ownLand.getHolder)
//	        enemyNumber += 1
//	    }
//	  }
//	  enemyNumber
//	}
//	
//
//	/**
//	 * Check if the player has to the declared position a neighbour which belongs to him.
//	 * @param world. The world of the game.
//	 * @return return true if the position has a neighbour which belongs another player.
//	 */
//	def checkHasWeakerEnemyNeighbour(world: Array[Array[Land]]):Boolean =
//	{
//	  var ownLand = this
//	  
//	  var neighbourContainer = new ArrayBuffer[WorldPosition]()
//	  
//	  neighbourContainer += new WorldPosition(position.row, position.column-1)
//	  neighbourContainer += new WorldPosition(position.row, position.column+1)
//	  neighbourContainer += new WorldPosition(position.row+1, position.column)
//	  neighbourContainer += new WorldPosition(position.row-1, position.column)
//	  
//	  var hasWeakerEnemyNeighbour = false
//	  neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
//
//	  if(neighbourContainer.length != 0)
//	  {
//	    for(i <- 0 until neighbourContainer.length)
//	    {
//	      var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
//	      if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType && 
//	          neighbourLand.getArmy < ownLand.getArmy)
//	      {
//	        hasWeakerEnemyNeighbour = true
//	      }
//	    }
//	  }else
//	    hasWeakerEnemyNeighbour = false     
//	    
//	    hasWeakerEnemyNeighbour
//	}
//
//	/**
//	 * Give the weakest neighbour as measured by the amound of army.
//	 * Are there more than one weakest neighbour because of equal of the army, 
//	 * the function will just return one of it per clockwise rotation => 1.top- 2.right- 3.down 4.left-land.
//	 * @param world. The world of the game.
//	 * @return return null if there is no land or the neighbour land is stronger as the ownland otherwise it will return the land.
//	 */
//	def getWeakestEnemyNeighbour(world: Array[Array[Land]]):Land =
//	{
//	  var ownLand = this
//	  
//	  var neighbourContainer = new ArrayBuffer[WorldPosition]()
//	  
//	  neighbourContainer += new WorldPosition(position.row+1, position.column)
//	  neighbourContainer += new WorldPosition(position.row, position.column+1)
//	  neighbourContainer += new WorldPosition(position.row-1, position.column)
//	  neighbourContainer += new WorldPosition(position.row, position.column-1)  
//	  
//	  
//	  var weakestNeighbour:Land = null
//	  neighbourContainer = neighbourContainer.filter(pos => World.checkPositionIsInWorld(pos))
//	  for(i <- 0 until neighbourContainer.length)
//
//	  if(neighbourContainer.length != 0)
//	  {
//	    var weakestArmy = ownLand.getArmy
//	    for(i <- 0 until neighbourContainer.length)
//	    {
//	      var neighbourLand = world(neighbourContainer.apply(i).row)(neighbourContainer.apply(i).column)
//	      if(neighbourLand.getHolder != ownLand.getHolder && neighbourLand.getFieldType && 
//	          neighbourLand.getArmy < ownLand.getArmy && neighbourLand.getArmy < weakestArmy)
//	      {
//	        weakestArmy = neighbourLand.getArmy
//	        weakestNeighbour = neighbourLand
//	      }
//	      
//	    }
//	  }else
//	    weakestNeighbour = null   
//	    
//	    weakestNeighbour
//	}
//}