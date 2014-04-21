package main.scala.util
import main.scala.model.Avatar

class Notification(var typ: Notification.NotificationTyp) 
{
  var map: String = ""
  var position: Position = null
  var value = 0;
  var currentPlayer:Avatar = null
  var message:String = ""
  var isOwnLand:Boolean = false

}
object Notification extends Enumeration 
{
    type NotificationTyp = Value
    val Map, Reinforcement, Battle, Position, Message= Value
}