package test.scala.util

import org.specs2.mutable._
import main.scala.util.FileUtil

class FileUtilSpec extends Specification
{
	"A FileUtil" should
	{
	  "be able to read data from a file" in
	  {
	    val fu = new FileUtil
		var file = "testtext"
		var outArray = fu.readData(file)
		var line = outArray.apply(0)
		line must be_==("Das ist ein Test.")
	  }
	}
}