package main.scala.view.swing

// scala packages
import scala.swing._
import scala.swing.BorderPanel.Position._

case class PlayerInit(playerAmount: String, botAmount: String)


/*
 * A pop up input menu for requesting the user to put the amount of human and bot player
 * */
class DialogMessagePanel extends Dialog 
{
	var playInit: Option[PlayerInit] = None
	val playerAmount = new TextField("2")
	val botAmount = new TextField("1")
	title = "Nachricht"
	modal = true
	// Panel contents
	contents = new BorderPanel 
	{
		layout(new BoxPanel(Orientation.Vertical) 
		{
			border = Swing.EmptyBorder(5,5,5,5)
		      contents += new Label("Anzahl Spieler:")
		      contents += playerAmount
		      contents += new Label("Anzahl Bots:")
		      contents += botAmount
	    }) = Center
	
	    layout(new FlowPanel(FlowPanel.Alignment.Right)(
	    	Button("OK") 
	    	{
	    		if(getInput()) 
	    		{
	    			playInit = Some(PlayerInit(playerAmount.text, botAmount.text))
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