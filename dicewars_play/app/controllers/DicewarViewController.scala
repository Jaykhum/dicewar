package controllers

import main.scala.dicewars
import main.scala.model._
import main.scala.model.Avatar._


class DicewarViewController() {
	val width = World.width
	val height = World.height
	val block = 3
	var name = "Daniel";
	var counter = 0;
	var game:Gamefield = null;

	def getCellSum() =
	{
		width*height
	}
	
	def getArmyForCell(id: Int):String =
	{
		var army = 0
		var column = 0
		var row = 0
		if(id != 0)
		{
		column = (id % width)
		row = (id / width)
		}
		army = game.world(row)(column).getArmy
		if(army == 0 && !game.world(row)(column).getFieldType)
			""
			else
		army.toString
	}
	
	def getPositionForCell(id: Int):WorldPosition = 
	{
		var column = 0
		var row = 0
		if(id != 0)
		{
		column = (id % width)
		row = (id / width)
		}
		new WorldPosition(row, column)
	}
	
	def getCellType(id: Int):String =
	{	
		var cellType:String = "waterCell"
		var column = 0
		var row = 0
		if(id != 0)
		{
		column = (id % width)
		row = (id / width)
		}
		
		if(game.world(row)(column).getFieldType)
					cellType = "landCell"
		if(game.fromLand != null && game.world(row)(column) == game.fromLand)
					cellType = "landCellSelected"
		else if(game.toLand != null && game.world(row)(column) == game.toLand)
			cellType = "landCellSelected"
					
		cellType
		
	}
	
	
	
	def getPlayerColor(id: Int):String =
	{	
		var cellColor:String = "black"
		var column = 0
		var row = 0
		if(id != 0)
		{
		column = (id % width)
		row = (id / width)
		}
		var colorString = "black"
		if(game.world(row)(column).getFieldType)
		{	
			var playerId = game.world(row)(column).getHolder
			
			var color = game.avatarContainer(playerId).color
			
			color match 
			{
			case null => "black"
			case `Blue` => colorString = "blue"
			case `Mangenta` => colorString = "mangenta"
			case `Green` => colorString = "green"
			case _ => colorString = "black"
			}
			
		}
		colorString
	}
	
	
	def checkAddNextLine(counter:Int):Boolean =
	{
	var nextLine = false
	if(counter == 0)
			return true
	if(counter % width == 0)
	{
		nextLine = true
		
	}else{
		nextLine = false
	}
	nextLine
	}
	
	
}