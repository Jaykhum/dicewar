package main.scala.util

import scala.io.Source
import scala.collection.mutable.MutableList
import scala.collection.mutable.StringBuilder

class FileUtil {

	def readData(filename: String) =  {
		var inputArray = new MutableList()
		var stBuilder =  new StringBuilder()
		for (line <- Source.fromFile(filename).getLines()) {
		  stBuilder.clear
		  inputArray.addString(stBuilder ++=(line))
		}
		inputArray
	}
}