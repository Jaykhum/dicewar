package main.scala.view.swing

import main.scala.controller.DicewarController
import scala.swing._
import scala.swing.event._
import scala.swing.event.MouseReleased
import java.awt.event.MouseEvent
import javax.imageio.ImageIO
import java.io.File

import main.scala.model.Land

class FieldPanel(controller:DicewarController) extends Panel  
{
	listenTo(mouse.clicks)
	val CellWidth:Int = 35
	val CellHeight:Int = 30
	val ImageWidth:Int = 119
	val ImageHeight:Int = 100
	
	reactions += 
	{
	  case e: MouseReleased => mouseReleasedHandler(e)
	}
	
	def mouseReleasedHandler(e:event.MouseReleased)
	{
	  if(e.peer.getButton() == MouseEvent.BUTTON1)
	  {
		  
	  }
	}
	
	override def paintComponent(g:Graphics2D):Unit =
	{
		g.setColor(new Color(255, 255, 255))
		g.fillRect(0, 0, controller.game.width, controller.game.height)
		// TODO schleife um alle felder zu zeichnen.
		for ( i <- 0 to controller.game.width-1; j <-0 to controller.game.height-1)
		{
			drawLogo(g, controller.game.world(j)(i))
		}
	}
	
	def drawLogo(g:Graphics2D, land:Land)
	{
		// x/y-pos berechnen
		val x = land.position.column * CellWidth
		val y = land.position.row *  CellHeight
		var image_land = ImageIO.read(new File("Symbols/" + "land" + ".png"))
		var image_water = ImageIO.read(new File("Symbols/" + "water" + ".png"))
		// g.drawImage(images("Logo"), x-pos, y-pos, null)
		//g.drawImage(image_water, x, y, null)
		g.drawImage(image_land, x, y, null)
	}
}