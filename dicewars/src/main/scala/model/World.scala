package main.scala.model

object World {
  
	lazy val height = 10
	lazy val width = 18;
	
	def checkPositionIsInWorld(position:WorldPosition):Boolean =
	{
	  if(position.row < 0 || position.row >  height-1 || position.column < 0 || position.column > width -1)
	    false
	  else
	    true
	}
}