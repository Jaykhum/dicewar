package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current
import main.scala._
import main.scala.util._
import main.scala.controller._
import main.scala.view._
import main.scala.model._
import play.api.mvc.SimpleResult
import scala.concurrent.Future
import play.api.libs.concurrent.Promise
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.libs.json.JsNumber
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json._

object Application extends Controller with View{
	var waitOfNotification:Boolean = true
	var ( enumerator, channel ) = Concurrent.broadcast[JsValue]
	var game:Gamefield = null
	val controller:DicewarViewController = new controllers.DicewarViewController
	//if server start a communication
	var isServerInitiator = false
	var isOnce = false
	var currentPlayer:Avatar = null
	var drawNotification:Boolean = false
	var waitPooling:Boolean = true

	var in = Iteratee.foreach[JsValue](println).map { _ =>
  }
  
 // var websocket = new Thread(new Runnable {
	//	  def run() {
		//    test2
		  //}
    //})
    //websocket.run()
	
//Console Test
//ws = new WebSocket('ws://localhost:9000/indexWebsocket')
//ws.onmessage = function( message ) { console.log( message ); };
//ws.send('test')


/**
def javascriptRoutes = Action { implicit request =>
  import routes.javascript._
  Ok(
    Routes.javascriptRouter("jsRoutes")(
    )
  ).as("text/javascript")
}
*/

def config = Action {

    Ok(views.html.config("Spieler Initialisierung"))	
}

def startView{}
def closeView{}

def index = Action {
    Ok(views.html.index("Index"))
}

def maps = Action {
	 Ok(views.html.maps("foo"))	
}

def map = Action{
Ok(views.html.map("map", controller))	

}



def index_websocket =  WebSocket.async[JsValue] { request =>

	if(!isServerInitiator){
	var counter = 0
		in = Iteratee.foreach[JsValue]{
		  json => counter += 1
		  if( json \ "type" != JsNull && Json.stringify(json \ "type") == "\"notification\"") 
		  {
				var notify: JsValue = json \ "data" \ "notify"
				Json.stringify(notify) match {
					case "\"ready\""	=>	dicewars.main(this)
					case "\"config\""	=>	sendPlayerConfig(Json.stringify(json \ "data" \ "player"), Json.stringify(json \ "data" \ "bot"))
					case "\"map\""		=> sendMapChoice(Json.stringify(json \ "data" \ "mapId"));
					case "\"position\"" => sendPosition(Json.stringify(json \ "data" \ "position"))
					case "\"answer\""	=> sendAnswer(Json.stringify(json \ "data" \ "answer"))
					case "\"amount\""	=> sendAmountOfUnit(Json.stringify(json \ "data" \ "amount"))
					case _ => println("Andere Notification");
				}
		  }else if(json \ "type" != JsNull && Json.stringify(json \ "type") == "\"pageCall\"") 
		  {
				var html: JsValue = json \ "type"  
		  }
		}	
	}else
	{
		isServerInitiator = false
	}

	//Future.successful(( Iteratee.ignore[String], enumerator ))
	Future.successful(( in, enumerator ))
}



override def updateObserver(notification:Notification)
   {
     notification.typ match
	   {
       	  case Notification.PlayerInit	=> 	playerInitHandler
		  case Notification.MapSample	=>	mapsHandler
		  case Notification.DrawUI		=>	mapHandler(notification)	
		  case Notification.Message		=>	messageHandler(notification)
		  case Notification.Question	=>	questionHandler
		  case Notification.Move		=>	moveHandler
		  case _ => println("Debug: Falsche Notification" + notification.typ)
	   }
   }
	
	def reinforcmentHandler(notification:Notification)
	{
		currentPlayer = notification.currentPlayer
	}
	
	def sendPlayerConfig(player: String, bot: String)
	{
	
		var notify = new Notification(Notification.PlayerInit)
		notify.inputType = "playerInit"
		notify.playerCount = (player.replace( "\"", "" )).toInt
		notify.botCount = (bot.replace( "\"", "" )).toInt
		notifyObservers(notify)
	}
  
	def sendMapChoice(id: String) = 
   {
   var mapName = ""
   id match
	   {
       	  case "\"1\"" => mapName = "basicland"
		  case "\"2\"" => mapName = "land2"
		  case "\"3\"" => mapName = "land3"
		  case "\"4\"" => mapName = "land4"
	   }
   
     var notify = new Notification(Notification.Map)
	 notify.inputType = "map"
     notify.map = mapName
     notifyObservers(notify)
   }
   
   
   def sendPosition(id: String)
   {
   
      var n = new Notification(Notification.Position)  
      n.position = controller.getPositionForCell((id.replace( "\"", "" )).toInt)
      n.inputType = "position"
      notifyObservers(n)
   }
   
   def sendAnswer(answer:String)
   {
	  var n = new Notification(Notification.Answer)
      n.answer = (answer.replace( "\"", "" ).toBoolean)
      n.inputType = "question"
      notifyObservers(n)
   }
   
   def sendAmountOfUnit(amount: String)
   {
	
	  var n = new Notification(Notification.Move)
	  n.amount = (amount.replace( "\"", "" )).toInt
      n.inputType = "amount"
      notifyObservers(n)
   }
   

  def playerInitHandler
  {

	  isServerInitiator = true
	  
	  controller.game = game
	  
	  val menuJsValue = Json.obj(
		  "type" -> JsString("pageCall"),
		  "data" -> Json.obj(
							"call" -> "config")
		  )
	    
	  val enumMenu: Enumerator[JsValue] = {Enumerator(menuJsValue)}
	 
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)
		}
		enumMenu(i)

  }
  
  
    def questionHandler
  {

	  isServerInitiator = true
	  
	  val questionJsValue = Json.obj(
		  "type" -> JsString("question"),
		  "data" -> Json.obj("content" -> "") 
		  )
	    
	  val enumMenu: Enumerator[JsValue] = {Enumerator(questionJsValue)}
	 
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)
		}
		enumMenu(i)

  }
  
  
  
	def mapsHandler
	{
	  isServerInitiator = true

		val mapsJsValue = Json.obj(
		  "type" -> JsString("pageCall"),
		  "data" -> Json.obj("call" -> "maps") 
		  )
		  
	  
		val enumMenu: Enumerator[JsValue] = {Enumerator(mapsJsValue)}
	   
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)
		}
		enumMenu(i)
	}
  
    def mapHandler(n:Notification)
	{

	  isServerInitiator = true
	  
	  val mapJsValue = Json.obj(
		  "type" -> JsString("pageCall"),
		  "data" -> Json.obj(
							"call" -> "map") 
		  )

		  
		  
	  val enumMenu: Enumerator[JsValue] = {Enumerator(mapJsValue)}
	   
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)

		}
		enumMenu(i)

	}
	
	def moveHandler 
	{

	  isServerInitiator = true
	  
	  val moveJsValue = Json.obj(
		  "type" -> JsString("move"),
		  "data" -> Json.obj("content" -> "") 
		  )
		  
		  
	  val enumMenu: Enumerator[JsValue] = {Enumerator(moveJsValue)}
	   
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)

		}
		enumMenu(i)

	}
	
	def messageHandler(n:Notification)
	{
	
	  isServerInitiator = true
	  
	  
	 var messageTyp:Message.MessageTyp = n.message.typ
     var messageContent:String = n.message.content
	 var messageTypeString = ""
	 var color = "black"
	 
	 
	 
     messageTyp match
	   {
		  case Message.Success => messageTypeString = "Success"; color = "green"
		  case Message.Error => messageTypeString = "Error"; color = "red"
		  case Message.Info => messageTypeString = "Info"; color = "black"
		  case Message.Player => messageTypeString = "Player"; color = avatarColor(n.currentPlayer.color)
		  case _ => println("Debug: Falsche Notification")
	   }
	  
	  
	  
	  
	  val mapJsValue = Json.obj(
		  "type" -> JsString("message"),
		  "data" -> Json.obj(
							"content" -> n.message.content,
							"status" -> messageTypeString,
							"color" -> color) 
		  )
  
	  val enumMenu: Enumerator[JsValue] = {Enumerator(mapJsValue)}
	   
		var i = Iteratee.foreach[JsValue]{
		msg => 
		channel.push(msg)

		}
		enumMenu(i)	
	}
	
	def avatarColor(color:Avatar.ColorTyp):String = 
	{
		var wuiColor = ""
	 color match 
     {
       case Avatar.Blue => wuiColor = "blue"
       case Avatar.Mangenta => wuiColor = "mangenta"
       case Avatar.Green => wuiColor = "green"
       case _ => println("Color Fehler")
     } 
	 wuiColor
	}
  
  
  def refresh
  {
  
  	val tempenum: Enumerator[String] = {Enumerator("Guillaume")}
  
	waitOfNotification = false
	var i = Iteratee.foreach[String]{
      msg => 
	}
	
	tempenum(i)
	
  }
  
  
  def tasks = TODO
  
  def newTask = TODO
  
  def deleteTask(id: Long) = TODO

}