package main.scala
import main.scala.controller.DicewarController
import main.scala.view.TUI
import main.scala.util.FileUtil
import java.io.File
import scala.collection.mutable.MutableList

object dicewars {
    def main(args: Array[String]) {
//    val controller=new DicewarController()
//    val tui=new TUI(controller)
    //val gui = new SwingGui(controller)

    //while(tui.processInputLine(readLine())){}
     println("moep")
     val fu = new FileUtil
     //val file =  new File("C:\\study\\workspace\\dicewars\\Maps\\basicland")
     var outArray = fu.readData("C:\\study\\workspace\\dicewars\\Maps\\basicland").toArray
     var ind:Int = 0
     for(ind <-  1 to outArray.length) println(outArray(ind))
  }
}