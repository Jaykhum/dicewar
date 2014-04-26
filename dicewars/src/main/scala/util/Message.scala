package main.scala.util

class Message(var typ: Message.MessageTyp, var content:String) 
{
}

object Message extends Enumeration 
{
    type MessageTyp = Value
    val Error, Success, Info = Value
}
