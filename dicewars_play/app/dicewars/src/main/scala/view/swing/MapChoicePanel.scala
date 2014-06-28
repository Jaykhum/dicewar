package main.scala.view.swing

// scala packages
import scala.swing._
import scala.swing.event.Event
import javax.swing.ImageIcon


// event class
case class MapSelectedEvent(val mapName:String) extends Event


/*
 * A 5x1 GridPanel which display the example of the chooseable maps
 * @ headline: label of this panel
 * */
abstract class MapChoicePanel(headline:String) extends GridPanel(5,1)
{
	// header
	val label = new Label(headline)
	val menuPanel = this
	// directory path for all icons-images
	val path:String = "Symbols/"
	// spe. path for one icon
	var iconPath:String = ""
	label.peer.setFont(new Font("Verdana", 1, 24))
	
	// Panel contents
	contents += label
	// button for map basic 
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
	
	// button for map land 2
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
	
	// button for map land 3
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

	// button for map land 4
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
	
	// event
	def notification(mapName:String) : Event
	
}