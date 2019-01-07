package v2

import java.io._

import config.AcousticProperty
import offline.AsciiOutput

/**
  * Created by wanganran on 9/12/18.
  */
class OfflineLogger(destpath:String) {
  private val datastream=new FileOutputStream(destpath+"data.txt")
  private val controlstream=new FileWriter(destpath+"control.txt")
  private val drawstream=new FileWriter(destpath+"draw.txt")
  private var currentId=0

  def put(data:Array[Byte]): Int ={
    datastream.write(data)
    datastream.flush()
    currentId+=1
    currentId-1
  }
  def put(reset:Boolean, cali:Boolean): Unit ={
    controlstream.write(currentId.toString+"\n")
    val control1=if(reset) 10 else 20
    val control2=if(cali)1 else 2
    controlstream.write((control1+control2).toString+"\n")
    controlstream.flush()
  }
  def put(draw:Boolean):Unit={
    drawstream.write(currentId.toString+"\n")
    drawstream.write(if(draw) "1\n" else "0\n")
    drawstream.flush()
  }

  def close(): Unit ={
    controlstream.close()
    datastream.close()
  }
}
class OfflineReader(destpath:String){
  private val datastream=new FileInputStream(destpath+"data.txt")
  private val controlstream=new BufferedReader(new FileReader(destpath+"control.txt"))
  private val drawstream=new BufferedReader(new FileReader(destpath+"draw.txt"))

  var controls=List[(Int, Boolean, Boolean)]()
  var draws=List[(Int, Boolean)]()

  var eof=false
  while(!eof) {
    val idstr = controlstream.readLine()
    if (idstr == null) eof=true
    else{
      val controlstr=controlstream.readLine()
      val id=idstr.toInt
      val reset=controlstr.charAt(0)=='1'
      val cali=controlstr.charAt(1)=='1'
      controls:+=(id, reset, cali)
    }
  }

  eof=false
  while(!eof) {
    val idstr = drawstream.readLine()
    if (idstr == null) eof=true
    else{
      val controlstr=controlstream.readLine()
      val id=idstr.toInt
      val draw=controlstr.charAt(0)=='1'
      draws:+=(id, draw)
    }
  }

  private var currentId=0

  def fetch()= {
    val barr = new Array[Byte](AcousticProperty.VER2_BUFFER_SIZE_BYTE_STATION)
    val r = datastream.read(barr)
    if (r <= 0) null
    else {
      currentId += 1
      var actiondraw = false
      var actionstop = false
      while (draws.nonEmpty && draws.head._1 == currentId - 1) {
        actiondraw = draws.head._2
        actionstop = !draws.head._2
        draws = draws.tail
      }
      if (controls.nonEmpty && controls.head._1 == currentId - 1) {
        val result = (currentId - 1, actiondraw, actionstop, controls.head._2, controls.head._3, barr)
        controls = controls.tail
        result
      }
      else {
        (currentId - 1, actiondraw, actionstop, false, false, barr)
      }
    }
  }
}
