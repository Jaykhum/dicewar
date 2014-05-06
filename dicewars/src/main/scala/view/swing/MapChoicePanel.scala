package main.scala.view.swing

import scala.swing._
import javax.swing.ImageIcon
import main.scala.util.Notification
import main.scala.util.Observable
import scala.swing.event.Event


case class MapSelectedEvent(val mapName:String) extends Event

abstract class MapChoicePanel(headline:String) extends GridPanel(5,1)
{
	val menuPanel = this
	val label = new Label(headline)
	val path:String = "Symbols/"
	var iconPath:String = ""
	label.peer.setFont(new Font("Verdana", 1, 24))
	

	contents += label
	contents += new Button("basic")
	{
		menuPanel .listenTo(this)
		action =  Action("Basicland")
		{
			  menuPanel.publish(notification("basicland"))
		}
		iconPath = path + "basicland.png"
		icon = new ImageIcon(iconPath)
		iconTextGap = 20
		focusPainted = false
	}
	
	contents += new Button("land2")
	{
		this.listenTo(this)
		action =  Action("land 2")
		{
			menuPanel.publish(notification("land2"))
		}
		iconPath = path + "land2.png"		
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		focusPainted = false
	}
		
	contents += new Button("land3")
	{
		this.listenTo(this)
		action =  Action("land 3")
		{
			 menuPanel.publish(notification("land3"))
		}
		iconPath = path + "land3.png"
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		bounds
		focusPainted = false
	}

	contents += new Button("land4")
	{
		this.listenTo(this)
		action =  Action("Land 4")
		{
			  menuPanel.publish(notification("land4"))
		}
		iconPath = path + "land4.png"
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		focusPainted = false
	}
	
	 def notification(mapName:String) : Event
	
}