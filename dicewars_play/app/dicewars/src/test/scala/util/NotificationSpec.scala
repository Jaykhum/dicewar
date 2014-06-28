package test.scala.util

// scala packages 
import org.specs2.mutable._

// own costum packages
import main.scala.util.Notification




/*
 * Spec class for testing the Notification
 * */
class NotificationSpec extends Specification
{
	"A Notification" should
	{
	  val testNotification =  new Notification(Notification.Answer) 
	  
	  "have a value map" in
	  {
	    testNotification.map must be_==("")
	  }
	  
	  "have a value position" in
	  {
	    testNotification.position must be_==(null)
	  }
	  
	  "have a value amount" in
	  {
	    testNotification.amount must be_==(0)
	  }
	  
	  "have a value playerCount" in
	  {
	    testNotification.playerCount must be_==(0)
	  }
	  
	  "have a value botCount" in
	  {
	    testNotification.botCount must be_==(0)
	  }
	  
	  "have a value currentPlayer" in
	  {
	    testNotification.currentPlayer must be_==(null)
	  }
	  
	  "have a value message" in
	  {
	    testNotification.message must be_==(null)
	  }
	  
	  "have a value answer" in
	  {
	    testNotification.answer must beFalse
	  }
	  
	  "have a value inputType" in
	  {
	    testNotification.inputType must be_==("")
	  }
	}
}