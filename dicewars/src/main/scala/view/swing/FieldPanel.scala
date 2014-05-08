package main.scala.view.swing

import main.scala.model.Gamefield
import scala.swing._
import scala.swing.event._
import scala.swing.event.MouseReleased
import java.awt.event.MouseEvent
import javax.imageio.ImageIO
import java.io.File
import main.scala.model.Land
import javax.swing.border.EmptyBorder
import java.awt.BorderLayout
import java.awt.Color
import main.scala.model.WorldPosition

case class FieldSelectedEvent(val position:WorldPosition) extends Event

class FieldPanel(game:Gamefield) extends Panel  
{
	listenTo(mouse.clicks)
	val image_land = ImageIO.read(new File("Symbols/land.png"))
	val image_water = ImageIO.read(new File("Symbols/water1.png"))
	val CellWidth:Int = 36
	val CellHeight:Int = 32
	updateSize
	
	reactions += 
	{
	  case e: MouseReleased => mouseReleasedHandler(e)
	}
	
	def findLand(p: Point):WorldPosition =
	{
		val col:Int = p.x / CellWidth
		val row:Int = p.y / CellHeight
		if(col >= 0 && row >= 0)
			new WorldPosition(row, col)
		else
			null
	}
	
	def mouseReleasedHandler(e:event.MouseReleased)
	{
	  if(e.peer.getButton() == MouseEvent.BUTTON1)
	  {
		  val landPosition:WorldPosition = findLand(e.point)
//		  println(in FP: e.point)
//		  println(in FP: landPosition.column + ","+ landPosition.row)
		  if (landPosition != null)
		  {
		    publish(new FieldSelectedEvent(landPosition))
		  }
	  }
	}
	
	def updateSize = 
	{
			/*
			preferredSize = new Dimension(
			    controller.game.width * ImageWidth,
			    controller.game.height * ImageHeight)
			*/
			preferredSize = new Dimension(
			    game.width * CellWidth,
			    (game.height) * CellHeight)
	}
	
	override def paintComponent(g:Graphics2D):Unit =
	{
		g.setColor(new Color(255, 255, 255))
		g.fillRect(0, 0, size.width, size.height)
		for ( i <- 0 to game.width-1; j <-0 to game.height-1)
		{
			drawLogo(g, game.world(j)(i))
		}
//		g.setColor(Color.RED)
//		g.setFont(new Font("Verdana", 1, 12))
//		g.drawString("Error Message", 0, (CellHeight * (game.height+1)))
		
	}
	
	def drawLogo(g:Graphics2D, land:Land)
	{
		// x/y-pos berechnen
		val x = land.position.column * CellWidth
		val y = land.position.row * CellHeight
		val offset_x = -5
		val offset_y = 4
		// g.drawImage(images("Logo"), x-pos, y-pos, null)
		if(land.getFieldType){
		  g.drawImage(image_land, x, y, null)
		  g.setColor(setPlayerColor(land))
		  g.setFont(new Font("Verdana", 1, 12))
		  g.drawString(land.getArmy.toString, (x+(CellWidth/2+ offset_x)), (y+(CellHeight/2+offset_y)))
		}
		else
		  g.drawImage(image_water, x, y, null)
		
	}
	
	def setPlayerColor(land:Land):Color =
	{
	  if(land.getHolder == 0)
	    Color.YELLOW
	  else if(land.getHolder == 1)
	  	Color.MAGENTA
	  else if(land.getHolder == 2)
	    Color.GREEN
	  else
	    Color.WHITE
	}
}