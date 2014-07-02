package test.scala.model

// scala packages 
import org.specs2.mutable._

// own costum packages
import main.scala.model.Dice

class DiceSpec extends SpecificationWithJUnit
{
	"A Dice" should
	{
	  var dice = new Dice
	   
	  "have a roll function " in
	  {
	    var randInt = dice.roll
	    randInt must be_!=(0)
	  }
	}	
}