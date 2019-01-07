package v2

import config.{AcousticProperty, SystemProperty}
import offline.{AsciiOutput}

/**
  * Created by anranw on 8/30/18.
  */
object OfflineMain extends App {
  val offline1=new AsciiOutput("/home/anranw/passive1.txt")
  val offline2=new AsciiOutput("/home/anranw/passive2.txt")
  val off1=new AsciiOutput("/home/anranw/act1.txt")
  val off2=new AsciiOutput("/home/anranw/act2.txt")
  val off3=new AsciiOutput("/home/anranw/act3.txt")
  val off4=new AsciiOutput("/home/anranw/act4.txt")

  def toFloat(b1:Byte, b2:Byte)=(b1&0xff)|((b2&0xff)<<8)
  def parseByteArr(data:Array[Byte], channel:Int, buffersize:Int)= {
    val result = Array.fill(channel)(new Array[Float](buffersize))
    for (j <- 0 until buffersize)
      for (i <- 0 until channel) {
        result(i)(j)=toFloat(data(j*2*channel+i*2),data(j*2*channel+i*2+1))
      }
    result
  }

  val server=new NetworkingServer(SystemProperty.VER2_PORT)
  server.start((time, data, conn)=>{
    println("receiving")
    val dataarr=conn.dev match {
      case SystemProperty.DEV_CLIENT =>
        parseByteArr(data, AcousticProperty.VER2_CHANNELS_CLIENT_CONFIG.length,
          AcousticProperty.VER2_BUFFER_SIZE_CLIENT)
      case SystemProperty.DEV_STATION =>
        parseByteArr(data, AcousticProperty.VER2_CHANNELS_CONFIG.length,
          AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE)
    }
    conn.dev match {
      case SystemProperty.DEV_CLIENT =>
        offline1.output(dataarr(0))
        offline2.output(dataarr(1))
      case SystemProperty.DEV_STATION =>
        off1.output(dataarr(0))
        off2.output(dataarr(1))
        off3.output(dataarr(2))
        off4.output(dataarr(3))
    }
  })

}
