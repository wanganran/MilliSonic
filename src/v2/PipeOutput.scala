package v2

import java.io._
import java.nio.{ByteBuffer, FloatBuffer}

/**
  * Created by anranw on 10/1/18.
  */
class PipeOutput(N:Int) {
  val pipes=Array.range(0, N).map{i=>
    new FileOutputStream("/tmp/pipeplot"+i.toString)
  }
  def output(i:Int, data:Array[Float], len:Int): Unit = {
    val buffer = ByteBuffer.allocate(len * 4)
    val floatBuffer = buffer.asFloatBuffer()
    floatBuffer.put(data, 0, len)
    pipes(i).write(buffer.array())
  }
}
