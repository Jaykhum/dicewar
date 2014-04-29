package main.scala.view.swing

import scala.swing._
import javax.swing.ImageIcon


class MapChoicePanel(headline:String) extends GridPanel(5,1) 
{
	val menuPanel = this
	val label = new Label(headline)
	val path:String = "Symbols/"
	var iconPath:String = ""
	label.peer.setFont(new Font("Verdana", 1, 24))
	contents += label
	contents += new Button
	{
		menuPanel .listenTo(this)
		action =  Action("Basicland")
		{
			  
		}
		//iconPath = path + "basicland.png"
		iconPath = path + "land.png"
		icon = new ImageIcon(iconPath)
		/*
		var s = new Dimension(icon.getIconHeight(), icon.getIconWidth())
		minimumSize = s
		preferredSize = s
		repaint
		* 
		*/
		requestFocus
		iconTextGap = 20
		focusPainted = false
	}
	
	contents += new Button
	{
		this.listenTo(this)
		action =  Action("land 2")
		{
			
		}
		//iconPath = path + "land2.png"		
		iconPath = path + "land.png"
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		focusPainted = false
	}
		
	contents += new Button
	{
		this.listenTo(this)
		action =  Action("land 3")
		{
			  
		}
		//iconPath = path + "land3.png"
		iconPath = path + "land.png"
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		bounds
		focusPainted = false
	}

	contents += new Button
	{
		this.listenTo(this)
		action =  Action("Land 4")
		{
			  
		}
		//iconPath = path + "land4.png"
		iconPath = path + "land.png"
		icon = new ImageIcon(iconPath.replace(".txt", ".png"))
		iconTextGap = 20
		focusPainted = false
	}
}