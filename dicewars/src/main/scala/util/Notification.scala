package main.scala.util
import main.scala.model.Avatar
import main.scala.model.Land

class Notification(var typ: Notification.NotificationTyp) 
{
  var map: String = ""
  var position: Position = null
  var value = 0;
  var currentPlayer:Avatar = null
  var message:String = ""
  var isOwnLand:Boolean = false
  var question:Boolean = false
  var isFirstLand = false
  var land:Land = null

}
object Notification extends Enumeration 
{
    type NotificationTyp = Value
    val Map, Reinforcement, Battle, Attack, Position, Message, Question, Tactic, Army, UI = Value
}