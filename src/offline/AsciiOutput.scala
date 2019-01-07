package offline

import java.io.FileWriter

class AsciiOutput(filename:String) {
  val file=new FileWriter(filename)

  def output(data:Array[Float]): Unit ={
    data.foreach(x=>file.write(x.toString+"\n"))
    file.flush()
  }

  def close(): Unit ={
    file.close()
  }
}
