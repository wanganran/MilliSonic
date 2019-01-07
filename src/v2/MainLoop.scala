package v2

import java.nio.file.FileSystemNotFoundException
import java.util.concurrent.ArrayBlockingQueue

import blocks.{FMCWAoA, FMCWFilter}
import blocks.FMCWFilter.{FMCWResult, StartPoint}
import config.{AcousticProperty, SystemProperty}
import utils.IQ

import scala.collection.mutable
import scala.io.StdIn

object MainLoop extends App {
  new MainLoop().start(null)

  var fid:Int=0

  var ydir=1
  var zdir=1
}

class MainLoop{

  class SmoothSample(N:Int){
    var buffer=List[(Float, Float, Float)]()

    def reset(): Unit ={
      buffer=List[(Float, Float, Float)]()
    }
    def smooth(sample:(Float, Float ,Float))={
      buffer:+=sample
      if(buffer.size>N)
        buffer=buffer.tail
      var result=(0f, 0f, 0f)
      for(s <- buffer)
        result=(result._1+s._1, result._2+s._2, result._3+s._3)
      result=(result._1/buffer.size, result._2/buffer.size, result._3/buffer.size)
      result
    }

  }

  private val PREHEAT=10

  def start(gUI: AbstractGUI): Unit ={

    val REPLAY=false

    val offlineLogger=if(!REPLAY)new OfflineLogger("log") else null



    val server=new NetworkingServer(SystemProperty.VER2_PORT)
    val drawServer=new DrawServer(SystemProperty.DRAW_PORT)
    val CHANNEL_NUM=AcousticProperty.VER2_CHANNELS_CONFIG.length
    val resultQueues=Array.fill(CHANNEL_NUM)(new ArrayBlockingQueue[FMCWResult](20))
    val threads=Array.range(0, CHANNEL_NUM).map(i=>new ChannelThread(i, result=>{
      resultQueues(i).put(result)
    }))

    val aoa=new FMCWAoA()

    var processing=false

    var reset=false
    var calibrate=false
    var drawstart=false
    var drawstop=false
    var draw=false

    var preheat=0

    MainLoop.ydir=1
    MainLoop.zdir=1

    def toFloat(b1:Byte, b2:Byte)=(b1&0xff)|((b2&0xff)<<8)
    def parseByteArr(data:Array[Byte])= {
      val result = Array.fill(CHANNEL_NUM)(new Array[Float](AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE))
      for (j <- 0 until AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE)
        for (i <- 0 until CHANNEL_NUM) {
          result(i)(j)=toFloat(data(j*2*CHANNEL_NUM+i*2),data(j*2*CHANNEL_NUM+i*2+1))
        }
      result
    }

    val smoothSample=new SmoothSample(2)

    val processingFunction:(Int, Array[Byte])=>Unit=(id:Int, data:Array[Byte])=>{
      if(drawstart){
        draw=true
        drawstart=false
        if(!REPLAY)offlineLogger.put(true)
      }
      if(drawstop){
        draw=false
        drawstop=false
        if(!REPLAY)offlineLogger.put(false)
      }
      if (reset) {
        threads.foreach(_.reset())
        aoa.reset()
        reset = false
        preheat = 0
        if(!REPLAY)offlineLogger.put(true, false)
      }
      if (calibrate) {
        aoa.resetAngleAndSpeed(0.15f)
        calibrate = false
        if(!REPLAY)offlineLogger.put(false, true)
      }
      if (processing) {
        if(!REPLAY)offlineLogger.put(data)
        val dataarr = parseByteArr(data)
        //println(id)
        MainLoop.fid=id
        for (i <- 0 until CHANNEL_NUM) {
          threads(i).enqueue(dataarr(i))
        }
        val results = Array.range(0, CHANNEL_NUM).map(i =>
          resultQueues(i).take())

        results(0) match {
          case FMCWFilter.StartPoint(p) =>
            val offsets = results.map {
              case FMCWFilter.StartPoint(p) => p
              case _ => -1
            }
            val medoff = offsets.sum / offsets.length
            threads.foreach(c => c.setOffset(medoff))
          case FMCWFilter.PhaseResult(phase) => {
            preheat += 1
            if (preheat > PREHEAT) {
              val tms = aoa.getTm(results.map {
                case FMCWFilter.PhaseResult(phase2) => phase2
                case _ => null
              })
              tms.zip(threads).foreach {
                case (tm, th) => th.setPeak(aoa.estimateFreqGivenTm(tm))
              }
              /*
              val (d, x, y) = aoa.AoA(tms)
              if (gUI != null) {
                val dd = d * AcousticProperty.SOUND_SPEED
                val rawsample=(-(dd * Math.cos(x) * Math.cos(y)).toFloat,
                  MainLoop.ydir*(dd * Math.sin(x)).toFloat,
                  MainLoop.zdir*(dd * Math.sin(y)).toFloat
                  )
                val sample=smoothSample.smooth(rawsample)
                println(sample)
                gUI.update(sample, draw)
              } else {
                println(d, x, y)
              }
              */
              val (x,y,z)=aoa.triangulate(tms)
              if(gUI!=null){
                val sample=smoothSample.smooth((z*AcousticProperty.SOUND_SPEED,
                  MainLoop.ydir*x*AcousticProperty.SOUND_SPEED,
                  MainLoop.zdir*y*AcousticProperty.SOUND_SPEED))
                println(sample)
                gUI.update(sample, draw)
              }else{
                println(x,y,z)
              }
            }
          }
        }
      }
    }

    val replayThread=new Thread() {
      override def run(): Unit = {
        val logReader = new OfflineReader("log")
        var eof = false
        Thread.sleep(1000)
        processing=true
        while (!eof) {
          val data = logReader.fetch()
          Thread.sleep(50)
          if (data == null) eof = true
          else {
            data match {
              case (_id, _drawstart, _drawstop, _reset, _cali, _data) =>
                reset = _reset
                calibrate = _cali
                drawstart=_drawstart
                drawstop=_drawstop
                processingFunction(_id, _data)
            }
          }
        }
      }
    }
    if(REPLAY){
      replayThread.start()
    } else {
      drawServer.start(draw=>{
        if(draw)drawstart=true
        else drawstop=true
      })
      server.start((_, data, _) => {
        processingFunction(0, data)
      }, x=>offlineLogger.close())
    }
    threads.foreach{_.start()}
    //print(gUI)
    if(gUI==null) {
      val ch = StdIn.readLine()
      processing = true
      val ch2 = StdIn.readLine()
      calibrate=true
    } else{
      gUI.callback={
        case 'Start => processing=true
        case 'Calibrate => calibrate=true
        case 'Reset => reset=true
      }
    }

  }


}