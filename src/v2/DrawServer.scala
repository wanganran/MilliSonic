package v2

import java.net.ServerSocket

/**
  * Created by anranw on 9/15/18.
  */
class DrawServer(port:Int) {
  private val server=new ServerSocket(port)

  def start(callback:Boolean=>Unit): Unit ={
    val serverThread=new Thread(){
      override def run(): Unit ={
        val socket=server.accept()
        println("draw client connected")
        val inputStream=socket.getInputStream
        var eof=false
        while(!eof){
          val action=inputStream.read()
          println("action", action)
          if(action<0)eof=true
          else
            callback(action=='1')
        }
        socket.close()
      }
    }
    serverThread.start()
  }
}
