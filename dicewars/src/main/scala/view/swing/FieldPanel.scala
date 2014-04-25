package main.scala.view.swing

import main.scala.controller.DicewarController
import scala.swing._
import scala.swing.event._
import scala.swing.event.MouseReleased
import java.awt.event.MouseEvent

class FieldPanel(controller:DicewarController) extends Panel  
{
	listenTo(mouse.clicks)
	
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
		g.setColor(new Color(255, 0, 0))
		g.fillRect(0, 0, controller.game.width, controller.game.height)
	}
	
	def drawLogo(g:Graphics2D)
	{
		//g.drawImage(images("Logo"))
	}
}