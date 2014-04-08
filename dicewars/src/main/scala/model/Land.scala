package main.scala.model

abstract class Land{
// Position
def showImage = "00"


}

class Field extends Land 
{
var army = 11		// number of units on the field
var holder = 0	// player-id who holds the field

def getArmy = army
def getHolder = holder

def setArmy(armyCount:Int) = this.army = armyCount
def setHolder(id:Int) = this.holder = id


 override def showImage = 
 { 
  if (army < 10)
	 "0" + army.toString
   else
     army.toString
 }

}

class Water extends Land 
{
  val sign = "ww"
  override def showImage = sign 
}