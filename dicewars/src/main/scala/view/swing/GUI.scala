package main.scala.view.swing

import main.scala.model.Gamefield
import main.scala.view.swing.FieldPanel
import main.scala.controller.DicewarController
import scala.swing._
import scala.swing.Swing.LineBorder
import scala.swing.event.WindowClosing

class GUI extends Frame {

	title = "Dicewars"
	var fieldPanel:FieldPanel =  null			
	private var controller:DicewarController = null
	reactions +=
	{
	  case WindowClosing(_) =>closeView
	}
	
	def startView(controller: DicewarController)
	{
		this controller = controller
		fieldPanel = new FieldPanel(controller)
		listenTo(fieldPanel)
	}

	val swingView = this
	menuBar = new MenuBar
	{
		contents += new Menu("Game")
		{
			contents += new MenuItem(Action("Neues Spiel starten")
			{
			  
			})
			contents += new MenuItem(Action("Runde beenden")
			{
			  
			})
			contents += new MenuItem(Action("Quit")
			{
			  closeView
			})
		}
		contents += new Menu("Actions"){}
	}

	def closeView 
	{
	    // dieses View aus der Liste entfernen.
	    dispose
	}
	/*
	val name:String = "Dicewars"
	val cells = Array.ofDim[CellPanel](game.height, game.width)
	
	def gridPanel = new GridPanel(game.height, game.width){
	  border = LineBorder(java.awt.Color.BLACK,2)
	  background = java.awt.Color.BLACK
	  for (i <- 0 to game.height; j <- 9 to game.width)
	  { 
		  contents += new Button(i.toString + " "+ j.toString)
	  }
	}
	
	//val statusline = new TextField(controller.statusText, 20)

	contents = new BorderPanel {
    //add(highlightpanel, BorderPanel.Position.North)
    add(gridPanel, BorderPanel.Position.Center)
    //add(statusline, BorderPanel.Position.South)
	}
	
	visible = true
	*/
	
}