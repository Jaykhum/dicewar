package main.scala.view.swing

import scala.swing._
import scala.swing.event.Event

case class CloseEvent extends Event
case class MapChoice extends Event

class MenuPanel(headline:String) extends GridPanel(4,1)
{
	val menuPanel =this
	val label = new Label(headline)

	label.peer.setFont(new Font("Verdana", 1, 24))
	contents += label
	contents += new Button
	{
		menuPanel.listenTo(this)

		action =  Action("Mapauswahl")
		{
			  menuPanel.publish(new MapChoice)
		}
		
	}
	
//	contents += new Button
//	{
//		menuPanel.listenTo(this)
//		action =  Action("Hilfe")
//		{
//			  //this.publish(notification)
//		}
//	}
		
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Quit")
		{
			  menuPanel.publish(new CloseEvent)
		}
	}
}