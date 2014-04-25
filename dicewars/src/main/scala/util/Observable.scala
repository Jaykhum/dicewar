package main.scala.util
import scala.collection.mutable.ArrayBuffer

trait Observer 
{
  def updateObserver(notification:Notification)
}
class Observable 
{
  var subscriberContainer:ArrayBuffer[Observer] = new ArrayBuffer[Observer]()
  def addObserver(subscriber:Observer) = subscriberContainer+=subscriber
  def removeObserver(subscriber:Observer) = subscriberContainer-=subscriber
  def notifyObservers(notification:Notification) = subscriberContainer.foreach(observer=>observer.updateObserver(notification))
}