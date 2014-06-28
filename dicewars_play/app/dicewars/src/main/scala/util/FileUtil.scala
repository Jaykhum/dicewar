package main.scala.util

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io.File

class FileUtil {

	def readData(filename: String) =  {
	val mapFile = new File(filename)
		var inputArray = new ArrayBuffer[String]()
		for (line <-  Source.fromFile(mapFile.getAbsolutePath()).getLines()) {
		  inputArray +=line
		}
		inputArray
	}
}