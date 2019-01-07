package v2

import java.net.ServerSocket

import NetworkingServer.Connection
import config.{AcousticProperty, SystemProperty}

import scala.io.BufferedSource

/**
  * Network protocol:
  * {
  * 4 byte: time
  * N*C*2 byte: data
  * }*
  */
object NetworkingServer{
  trait Connection{val IP:String; val dev:Int; def stop(); def sendback(data:Array[Byte], size:Int)}
}
class NetworkingServer(port:Int) {
  private val server=new ServerSocket(port)
  private var serverThread:Thread=null
  private val TIME_SIZE=4

  def start(handler:(Int, Array[Byte], Connection)=>Unit, onStop:Connection=>Unit=null): Unit ={
    def byteBufferToInt(b:Array[Byte])={
      (b(0)&0xff)|((b(1)&0xff)<<8)|((b(2)&0xff)<<16)|((b(3)&0xff)<<24)
    }
    serverThread=new Thread(()=>{
      while(true){
        println("waiting for client...")
        val s=server.accept()
        val in=s.getInputStream
        val out=s.getOutputStream

        var _stop=false
        var buffer:Array[Byte]=null
        val timeBuffer=new Array[Byte](TIME_SIZE)

        val devtype=in.read()

        println("1 client connected: "+devtype.toString)
        val conn=new Connection {
          val IP=s.getInetAddress.toString
          val dev=devtype
          def stop()=_stop=true
          def sendback(data:Array[Byte], size:Int): Unit ={
            if(!_stop)out.write(data, 0, size)
          }
        }

        val socketThread=new Thread(()=>{
          //val bufferSize=AcousticProperty.VER2_FREQ_DURATION_SAMPLE*AcousticProperty.VER2_CHANNELS_CONFIG.length*2
          val bufferSize=
            if(devtype==SystemProperty.DEV_STATION)
              AcousticProperty.VER2_BUFFER_SIZE_BYTE_STATION
            else
              AcousticProperty.VER2_BUFFER_SIZE_BYTE_CLIENT
          buffer=new Array[Byte](bufferSize)

          println("new connection, bufferSize: "+bufferSize)
          while(!_stop){
            var len=0
            while(len<TIME_SIZE && !_stop){
              val l=in.read(timeBuffer, len, TIME_SIZE-len)
              if(l<0){
                _stop=true
              }
              else{
                len+=l
              }
            }

            val time=byteBufferToInt(timeBuffer)


            len=0
            while(len<bufferSize && !_stop){
              val l=in.read(buffer, len, bufferSize-len)
              if(l<0){
                _stop=true
              }
              else{
                len+=l
              }
            }
            if(!_stop){
              handler(time, buffer, conn)
            }
          }
          println("Connection closed")
          if(onStop!=null)onStop(conn)
          s.close()
        })

        socketThread.start()
      }
    })
    serverThread.start()
  }
  def stop(): Unit ={
    server.close()
  }
  def waitUntilStop(): Unit ={
    if(serverThread!=null)
      serverThread.join()
  }
}
