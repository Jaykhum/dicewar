package main.scala.model

import scala.util.Random


/*
 * This class represents the Dice objects 
 * */
class Dice {
	
    var dice = new Random()
	var rnd:Int = 0
	
	/*
	 * Roll the dice and get the eye value
	 * @ return: the eye value  
	 * */ 
	def roll:Int = 
	{
	 rnd = dice.nextInt(5)+1
	 rnd
	}
}