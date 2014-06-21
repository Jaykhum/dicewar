package test.scala.model

import org.specs2.mutable._
import main.scala.model.Dice

class DiceSpec extends Specification 
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