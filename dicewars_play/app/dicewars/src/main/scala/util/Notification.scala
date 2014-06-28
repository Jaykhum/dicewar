package main.scala.util
import main.scala.model.Avatar
import main.scala.model.Land
import main.scala.model.WorldPosition


class Notification(var typ: Notification.NotificationTyp) 
{
	var map: String = ""
	var position: WorldPosition = null
	var amount = 0
	var playerCount = 0
	var botCount = 0
	var currentPlayer:Avatar = null
	var message:Message = null
	var answer:Boolean = false
	var inputType:String = ""
}

object Notification extends Enumeration 
{
    type NotificationTyp = Value
    val Answer, DrawUI, Exit, GameOver, Input, Map, MapSample, Message, Move, PlayerInit, Position, Question, Reset = Value
}