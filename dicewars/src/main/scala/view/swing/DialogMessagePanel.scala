package main.scala.view.swing

import scala.swing._
import scala.swing.BorderPanel.Position._

//case class UnitAmount(amount: String)

class DialogMessagePanel(message:String) extends Dialog 
{
	//val password = new PasswordField
	title = "Nachricht"
	modal = true
	// Panel contents
	contents = new BorderPanel 
	{
		layout(new BoxPanel(Orientation.Vertical) 
		{
			border = Swing.EmptyBorder(5,5,5,5)
			contents += new Label(message)
	    }) = Center
	
	    layout(new FlowPanel(FlowPanel.Alignment.Right)(
	    	Button("OK") 
	    	{
	    		if(getInput()) 
	    		{
	    			close()
	    		} 
	    		else 
	    		{
	    			Dialog.showMessage(this, "Eingabe ungueltig, eine Zahl wird benoetigt. Bitte wiederhole die Eingabe korrekt!", "Input Error", Dialog.Message.Error)
	    		}
	    	}
	    )) = South
	 }

  	def getInput() = true
  	// push-up-button displayposition 
	centerOnScreen()
  	// display
  	open()
}