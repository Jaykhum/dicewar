package main.scala.view.swing
import scala.swing._
import scala.swing.BorderPanel.Position._


case class UnitAmount(amount: String)

class DialogPanel extends Dialog {
  var amount: Option[UnitAmount] = None
  val unitCount = new TextField
  //val password = new PasswordField

  title = "Interaction"
  modal = true

  contents = new BorderPanel {
    layout(new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(5,5,5,5)

      contents += new Label("Bitte Anzahl Einheiten eingeben!")
      contents += unitCount
    }) = Center

    layout(new FlowPanel(FlowPanel.Alignment.Right)(
      Button("Eingabe") {
        if (makeLogin()) {
          amount = Some(UnitAmount(unitCount.text))
          close()
        } else {
          Dialog.showMessage(this, "Eingabe ungueltig, eine Zahl wird benoetigt. Bitte wiederhole die Eingabe korrekt!", "Input Error", Dialog.Message.Error)
        }
      }
    )) = South
  }

  def makeLogin() = true // here comes you login logic

  centerOnScreen()
  open()
}