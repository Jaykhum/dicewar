package test.scala.model

// scala packages
import org.specs2.mutable._

// own costum packages
import main.scala.model.Avatar
import main.scala.model.Field
import main.scala.model.Land
import main.scala.model.Water
import main.scala.model.WorldPosition


/*
 * Spec class for testing the models WorldPosition, Field and WaterField
 * */
class LandSpec extends Specification
{
	"A WorldPosition" should
	{	  
	  // test fields
	  val position1 = new WorldPosition(3,2)
	  val position2 = new WorldPosition(2,3)
	  val position3 = new WorldPosition(4,2)
	  
	  "have a row value" in
	  {
	    position1.row must be_==(3)
	  }
	  
	  "have a column value" in
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
		
		"be able to return an array with own neighbours is correct" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(7)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			var neighbourContainer = testField.getOwnNeighbourContainer(testWorld) 
			neighbourContainer.isEmpty must beFalse
		}
		
		"be able to return an array with own neighbours is wrong" in
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
			var testField =  new Field(5, 5)
			testField.setHolder(9)
			var neighbourContainer = testField.getOwnNeighbourContainer(testWorld) 
			neighbourContainer.isEmpty must beTrue
		}
		
		"be able to return an array with enemy neighbours is correct" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(2)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(5)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			var neighbourContainer = testField.getEnemyNeighbourContainer(testWorld) 
			neighbourContainer.isEmpty must beFalse
		}
		
		"be able to return an array with enemy neighbours is wrong" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(7)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			var neighbourContainer = testField.getEnemyNeighbourContainer(testWorld) 
			neighbourContainer.isEmpty must beTrue
		}
		
		"be able to count the amount of enemy neighbours is 2" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(2)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(5)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			var neighbourContainer = testField.getNumberOfEnemyNeighbour(testWorld) 
			neighbourContainer must be_==(2)
		}
		
		"be able to count the amount of enemy neighbours is 0" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(7)
			var testField =  new Field(4, 3)
			testField.setHolder(7)
			var neighbourContainer = testField.getNumberOfEnemyNeighbour(testWorld) 
			neighbourContainer must be_==(0)
		}
		
		"be able to check the enemy neighbours army count weaker" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(6)
			testWorld(3)(3).setArmy(20)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(9)
			testWorld(3)(3).setArmy(13)
			var testField =  new Field(4, 3)
			testField.setArmy(37)
			testField.setHolder(7)
			testField.checkHasWeakerEnemyNeighbour(testWorld) must beTrue
		}
		
		"be able to check the enemy neighbours army count stronger" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(6)
			testWorld(3)(3).setArmy(20)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(9)
			testWorld(5)(3).setArmy(13)
			var testField =  new Field(4, 3)
			testField.setArmy(2)
			testField.setHolder(7)
			testField.checkHasWeakerEnemyNeighbour(testWorld) must beFalse
		}
		
		"be able to return the weakest enemy neighbours Territory" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(6)
			testWorld(3)(3).setArmy(20)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(9)
			testWorld(5)(3).setArmy(13)
			var testField =  new Field(4, 3)
			testField.setArmy(37)
			testField.setHolder(7)
			var testEnemyField =testField.getWeakestEnemyNeighbour(testWorld) 
			testEnemyField must be_==(testWorld(5)(3))
			testEnemyField.getArmy must be_==(13)
		}
		
		"be fail to return the weakest enemy neighbours Territory because they are stronger" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(6)
			testWorld(3)(3).setArmy(20)
			testWorld(3)(2).setHolder(11)
			testWorld(5)(3).setHolder(9)
			testWorld(5)(3).setArmy(13)
			var testField =  new Field(4, 3)
			testField.setArmy(5)
			testField.setHolder(7)
			var testEnemyField = testField.getWeakestEnemyNeighbour(testWorld) 
			testEnemyField must be_==(null)
		}
		
		"be fail to return the weakest enemy neighbours Territory because no enemies" in
		{
			val testWorld = Array.ofDim[Land](size,size)
			for(i <- 0 until size-1; j <- 0 until size-1)
			  {
			    testWorld(i)(j) = new Water(i,j)
			  }
			testWorld(3)(3) = new Field(3, 3)
			testWorld(3)(2) = new Field(3, 2)
			testWorld(5)(3) = new Field(5, 3)
			testWorld(3)(3).setHolder(7)
			testWorld(3)(3).setArmy(20)
			testWorld(3)(2).setHolder(6)
			testWorld(5)(3).setHolder(7)
			testWorld(5)(3).setArmy(13)
			var testField =  new Field(4, 3)
			testField.setArmy(45)
			testField.setHolder(7)
			var testEnemyField = testField.getWeakestEnemyNeighbour(testWorld) 
			testEnemyField must be_==(null)
		}
	}
	
	"A WaterField" should
	{
	  
	  val testWaterField = new Water(1,1)
	  val testWaterField1 = new Water(1,2)
	  val testWaterField2 = new Water(4,1) 
	  
	  "have a row value" in
	  {
	    testWaterField.position.row must be_==(1)
	  }
	  
	  "have a column value" in
	  {
	    testWaterField.position.column must be_==(1)
	  }
	  
	  "have a sign value" in
	  {
	    testWaterField.sign must be_==("ww")
	  }
	  
	  "have a holder value" in
	  {
	    testWaterField.holder must be_==(-1)
	  }
	  
	  "be able to return the army on this field" in
	  {
	    testWaterField.getArmy must be_==(0)
	  }
	  
	  "be able to return the holder id of this field" in
	  {
	    testWaterField.getHolder must be_==(-1)
	  }
	  
	  "be able to return this field is no LandFieldType" in
	  {
	    testWaterField.getFieldType must beFalse
	  }
	  
	  "be able to return false then checking the holder" in
	  {
	    var testPlayer = new Avatar(3)
	    testWaterField.checkHolder(testPlayer) must beFalse
	  }
	  
	  "be able to check the neighbourhood with an other field is correct" in
	  {
	    testWaterField.checkNeighbourhood(testWaterField1) must beTrue
	  }
	  
	  "be able to check the neighbourhood with an other field is wrong" in
	  {
	    testWaterField.checkNeighbourhood(testWaterField2) must beFalse
	  }
	}
}