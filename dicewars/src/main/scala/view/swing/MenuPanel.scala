package main.scala.view.swing

import scala.swing._


class MenuPanel(headline:String) extends GridPanel(4,1)
{
	val menuPanel =this
	val label = new Label(headline)
	var mapSelect:MapChoicePanel = null
	label.peer.setFont(new Font("Verdana", 1, 24))
	contents += label
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Mapauswahl")
		{
			  mapSelect= new MapChoicePanel("Mapauswahl")
			  doMapSelect(mapSelect)
		}
		
	}
	
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Hilfe")
		{
			  //this.publish(notification)
		}
	}
		
	contents += new Button
	{
		menuPanel.listenTo(this)
		action =  Action("Quit")
		{
			  //this.publish(notification)
		}
	}
	
	
	def doMapSelect(panel:Panel)=
	{
		contents.clear
		visible = false
		//minimumSize = new Dimension(640, 480)
		contents +=  panel
		visible = true
	}
	
}