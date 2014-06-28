package main.scala.view
import main.scala.util._

trait View extends Observable with Observer
{
	def closeView
	def startView
}