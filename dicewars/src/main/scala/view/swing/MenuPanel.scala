package main.scala.view.swing

import scala.swing._
import scala.swing.event.Event

// event class
case class CloseEvent extends Event
case class MapChoice extends Event

/*
 * 
 * */
class MenuPanel(headline:String) extends GridPanel(4,1)
{
	val label = new Label(headline)
	val menuPanel = this
	label.peer.setFont(new Font("Verdana", 1, 24))

	// Panel contents
	contents += label
	// map menu button
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Mapauswahl")
		{
			  menuPanel.publish(new MapChoice)
		}
	}
	
	// help menu button
//	contents += new Button
//	{
//		menuPanel.listenTo(this)
//		action =  Action("Hilfe")
//		{
//			  //this.publish(notification)
//		}
//	}
	
	// quit button	
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Quit")
		{
			  menuPanel.publish(new CloseEvent)
		}
	}
}