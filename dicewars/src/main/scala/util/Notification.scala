package main.scala.util
import main.scala.model.Avatar
import main.scala.model.Land
import main.scala.model.WorldPosition


class Notification(var typ: Notification.NotificationTyp) 
{
  var map: String = ""
  var position: WorldPosition = null
  var value = 0;
  var currentPlayer:Avatar = null
  var message:Message = null
  var isFromLand:Boolean = false
  var question:Boolean = false
  var land:Land = null

}
object Notification extends Enumeration 
{
    type NotificationTyp = Value
    val Menu, Help, MapSample, Map, NewGame, Exit, Reinforcement, BattleAssign, BattleAttack, Message, Question, TacticAssign, TacticArmy, DrawUI, GameOver = Value
}