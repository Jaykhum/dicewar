package main.scala.model

import scala.util.Random

/*
class Dice {
  //TODO Richtiges Random momentan keine Zufallszahl
	var dice = new Random(100000)
	var rnd:Int = 0
	
	def roll:Int = 
	{
	 rnd = dice.nextInt(6)
	 rnd
	}
}
*/

// Random 

class Dice {
	var dice = new Random()
	var rnd:Int = 0
	
	def roll:Int = 
	{
	 rnd = dice.nextInt(5)+1
	 rnd
	}
}