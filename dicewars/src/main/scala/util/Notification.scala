package main.scala.util


class Notification(var typ: Notification.NotificationTyp) 
{
  var map: String = ""
  var position: Position = null

}
object Notification extends Enumeration 
{
    type NotificationTyp = Value
    val Map, Position= Value
}