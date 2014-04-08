package main.scala.util

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

class FileUtil {

	def readData(filename: String) =  {
		var inputArray = new ArrayBuffer[String]()
		for (line <-  Source.fromFile(filename).getLines()) {
		  inputArray +=line
		}
		
		inputArray
	}
}