package main.scala.view.swing

// scala packages
import scala.swing._
import scala.swing.BorderPanel.Position._


case class UnitAmount(amount: String)

/*
 * A pop up input menu for requesting the user to put the amount of the units that should be moved
 * */
class DialogPanel extends Dialog {
	var amount: Option[UnitAmount] = None
	val unitCount = new TextField
	title = "Interaction"
	modal = true
	// Panel contents
	contents = new BorderPanel {
    	layout(new BoxPanel(Orientation.Vertical) 
    	{
    		border = Swing.EmptyBorder(5,5,5,5)
    		contents += new Label("Bitte Anzahl Einheiten eingeben!")
    		contents += unitCount
    	}) = Center

    	layout(new FlowPanel(FlowPanel.Alignment.Right)(
    			Button("Eingabe") 
    			{
    				if (getInput()) 
    				{
    					amount = Some(UnitAmount(unitCount.text))
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