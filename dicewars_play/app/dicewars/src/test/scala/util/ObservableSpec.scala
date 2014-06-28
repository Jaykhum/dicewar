package test.scala.util

import org.specs2.mutable._

import main.scala.util.Observable
import main.scala.util.Observer
import main.scala.util.Notification

class ObservableSpec extends Specification
{
	val publisher = new Observable{}
	val subscriber = new Observer{
		var observerStatus = false
		override def updateObserver(notification:Notification)
		{
			notification.typ match
			{
				case Notification.Answer => observerStatus = true
			}
		}
	}
	"An observer" should
	{
		"be able to update an Observer" in
		{	
			subscriber.observerStatus must beFalse
			var testNotification = new Notification(Notification.Answer)
			subscriber.updateObserver(testNotification)
			subscriber.observerStatus must beTrue

		}
	}
	
	"An observable" should
	{
		"can add and remove observers" in
		{      
		    publisher.addObserver(subscriber)
		    publisher.subscriberContainer.head must be_==(subscriber)
		    publisher.removeObserver(subscriber)
		    publisher.subscriberContainer.isEmpty must beTrue
		}
    
		"can notifiy observers" in
		{
			
			subscriber.observerStatus must beFalse
			publisher.addObserver(subscriber)
			var testNotification = new Notification(Notification.Answer)
			publisher.notifyObservers(testNotification)
			publisher.removeObserver(subscriber)
			subscriber.observerStatus must beTrue
		}
	}
}