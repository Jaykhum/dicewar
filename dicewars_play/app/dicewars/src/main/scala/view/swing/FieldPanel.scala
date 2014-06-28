package main.scala.view.swing

// scala packages 
import scala.swing._
import scala.swing.event._
import scala.swing.event.MouseReleased
import java.awt.event.MouseEvent
import javax.imageio.ImageIO
import java.io.File
import javax.swing.border.EmptyBorder
import java.awt.BorderLayout
import java.awt.Color

// own costum packages
import main.scala.model.Gamefield
import main.scala.model.Land
import main.scala.model.WorldPosition
import main.scala.model.World


// event class
case class FieldSelectedEvent(val position:WorldPosition) extends Event


/*
 * panel for displaying the gamefield 
 * @ game: model of the gamefield
 * */
class FieldPanel(game:Gamefield) extends Panel  
{
	listenTo(mouse.clicks)
	// paths to imagefiles for the field labels 
	val image_land = ImageIO.read(new File("app/dicewars/Symbols/land.png"))
	val image_water = ImageIO.read(new File("app/dicewars/Symbols/water1.png"))
//	val image_selected = ImageIO.read(new File("app/dicewars/Symbols/selected.png"))
	
	// size of one field
	val CellWidth:Int = 36
	val CellHeight:Int = 32
	val TextHeight:Int = 15
	val TextOffset:Int = 3
	var msgOffset:Int = 0
	var messageText = Array[String]("", "", "", "", "")
	var messageColor = Array[Int](1, 1, 1, 1, 1)
	
	updateSize
	
	/*
	 * actions corresponding the mouse actions
	 * */
	reactions += 
	{
	  case e: MouseReleased => mouseReleasedHandler(e)
	}
	

	/*
	 * compute field index
	 * @ p: position of the mouse
	 * */
	def findLand(p: Point):WorldPosition =
	{
		val col:Int = p.x / CellWidth
		val row:Int = p.y / CellHeight
		val rect = new Rectangle(p.x, p.y, CellWidth, CellHeight)
      	if (rect.contains(p)) {
      		if(col >= 0 && row >= 0)
      		{
      			return new WorldPosition(row, col)
      		}
      	}
		return null
	}
	
	
	/*
	 * event handler for mouse action
	 * @ e: event of which should be reacted
	 * */
	def mouseReleasedHandler(e:event.MouseReleased)
	{
		if(e.peer.getButton() == MouseEvent.BUTTON1)
		{
			val landPosition:WorldPosition = findLand(e.point)
			if (landPosition != null)
			{
				publish(new FieldSelectedEvent(landPosition))
			}
		}
		repaint
	}
	
	
	/*
	 * change size of the frame
	 * */
	def updateSize = 
	{
		preferredSize = new Dimension(
			World.width * CellWidth,
			(World.height + TextOffset) * CellHeight)
	}
	
	
	/*
	 * display all components of this pannel
	 * @ g: the window of the frame
	 * */
	override def paintComponent(g:Graphics2D):Unit =
	{
		g.setColor(new Color(255, 255, 255))
		g.fillRect(0, 0, size.width, size.height)
		for ( i <- 0 to World.width-1; j <-0 to World.height-1)
		{
			drawLogo(g, game.world(j)(i))
		}

		/*
		 * Display the last five game messages for the user
		 * */
		g.setFont(new Font("Verdana", 1, 12))
		for( i <- 0 to 4)
		{
		  g.setColor(matchMsgColor(messageColor(i)))
		  g.drawString(messageText(i), 0, (CellHeight * World.height + (TextHeight*(i+1))))
		}
	}
	
	
	/*
	 * draw the field logo
	 * @ g: the window of the frame
	 * @ land: contains the territory information
	 * */
	def drawLogo(g:Graphics2D, land:Land)
	{
		// x/y-pos berechnen
		val x = land.position.column * CellWidth
		val y = land.position.row * CellHeight
		val offset_x = -5
		val offset_y = 4
		/*
		 *  draw the logo for this field
		 *  g.drawImage(images("Logo"), x-pos, y-pos, null)
		 *  draw the units amount on this field in the color of the player who owns them
		 *  if it's a land draw a land-logo else draw the water-logo
		 */
		if(land.getFieldType){
			if (land == game.fromLand || land == game.toLand)
				g.drawImage(image_land, x, y, 1, 1, null)
//			{
//				g.drawImage(image_selected, x, y, null)
//				selectedLand == null
//			}
			else
				//g.drawImage(image_land, x, y, CellWidth, CellHeight, null)
				g.drawImage(image_land, x, y, null)
		  	g.setColor(setPlayerColor(land))
		  	g.setFont(new Font("Verdana", 1, 12))
		  	g.drawString(land.getArmy.toString, (x+(CellWidth/2+ offset_x)), (y+(CellHeight/2+offset_y)))
		}
		else
			g.drawImage(image_water, x, y, null)
	}
	
	
	/*
	 * specify the player who owns the army with his color
	 * @ land: contains the holder information which is needed for the colorselection
	 * */
	def setPlayerColor(land:Land):Color =
	{
		 land.getHolder match{
		   case 0 => Color.BLUE
		   case 1 => Color.MAGENTA
		   case 2 => Color.GREEN
		   case _ => Color.WHITE
		 }
	}
	
	
	/*
	 * take the game message and save the last five ones to display
	 * @ message: content of the message
	 * @ outType: defines the color
	 * */
	def showMsg(message:String, outType:Int)
	{	
		if((msgOffset %5) == 0)
		  msgOffset = 0
		messageColor(msgOffset) = outType
		messageText(msgOffset) = message
		msgOffset += 1
		repaint
	}
	
	/*
	 * handle the color which should be displayed
	 * */
	def matchMsgColor(outType:Int):Color =
	{
	  outType match
	  {
	    case 0 => Color.BLACK
	    case 1 => Color.RED
	    case 2 => Color.BLUE
	    case 3 => Color.GREEN
	    case 4 => Color.MAGENTA
	    case 5 => Color.YELLOW
	    case _ => Color.BLACK
	  }
	}
}