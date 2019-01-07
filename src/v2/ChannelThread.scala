package v2

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import blocks.FMCWFilter
import offline.AsciiOutput
import utils.IQ


/**
  * Created by anranw on 5/23/18.
  */
class ChannelThread(id:Int, callback:FMCWFilter.FMCWResult=>Unit) extends Thread {
  //private val fileOutput=new AsciiOutput("/Users/wanganran/channel"+id.toString+".txt")

  private val inputQueue=new ArrayBlockingQueue[Option[Array[Float]]](20)
  private val fmcwFilter=new FMCWFilter(id)

  def enqueue(data:Array[Float]): Unit ={
    inputQueue.put(Some(data))
  }

  def setOffset(offset:Int): Unit ={
    fmcwFilter.adjustOffset(offset)
  }

  def setPeak(peak:Float): Unit ={
    fmcwFilter.updatePeak(peak)
  }

  def shutdown(): Unit ={
    inputQueue.put(None)
  }
  def reset(): Unit={
    inputQueue.put(Some(new Array[Float](0)))
  }
  override def run(): Unit = {
    var running = true
    while (running) {
      inputQueue.take() match {
        case None => {
          running = false
          fmcwFilter.reset()
        }
        case Some(buffer) if buffer.length==0 => {
          fmcwFilter.reset()
        }
        case Some(buffer) => {
          //fileOutput.output(buffer)
          callback(fmcwFilter.input(buffer))
        }
      }
    }
  }
}
