package test.scala.model

import org.specs2.mutable._
import main.scala.model.World
import main.scala.model.WorldPosition

class WorldSpec extends Specification
{
	"A World" should
	{	  
	  "be able to check Position is in World" in
	  {
		  var position = new WorldPosition(3,8)
		  World.checkPositionIsInWorld(position) must beTrue
	  }
	  
	  "be able to check Position is out World" in
	  {
		  var position = new WorldPosition(-3,8)
		  World.checkPositionIsInWorld(position) must beFalse
	  }
	  
	  "have height = 10" in
	  {
		  World.height must be_==(10)
	  }
	  
	  "have width =  18" in
	  {
		  World.width must be_==(18)
	  }
	}
}