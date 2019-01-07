/**
  * Created by anranw on 5/4/18.
  */
package object utils {
  def int(x:Float)=(if(x > -0.001f)x+0.001f else x-0.001f).asInstanceOf[Int]
  def int(x:Double)=(if(x> -0.001)x+0.001 else x-0.001).asInstanceOf[Int]
  def argmax(x:Array[Float], len:Int = 0)={
    var id=0
    for(i<-1 until (len+x.length-1)%x.length+1)
      if(x(i)>x(id))id=i
    id
  }
  def argmin(x:Array[Float],start:Int=0, len:Int=0)={
    var id=start
    val _len=if(len>0) Math.min(x.length-start, len) else x.length-start
    for(i<-1 until _len)
      if(x(i+start)<x(id))id=i+start
    id
  }
  def getAngle(i:Float, q:Float)=Math.atan2(q, i).toFloat

  case class IQ(amp:Float, angle:Float){
    def *(iq:IQ)=IQ(amp*iq.amp, angle+iq.angle)
    def +(iq:IQ)={
      val i=amp*Math.cos(angle)
      val q=amp*Math.sin(angle)
      val i2=iq.amp*Math.cos(iq.angle)
      val q2=iq.amp*Math.sin(iq.angle)
      val i3=i+i2
      val q3=q+q2
      IQ(Math.sqrt(i3*i3+q3*q3).asInstanceOf[Float], Math.atan2(q3, i3).asInstanceOf[Float])
    }
    def -(iq:IQ)=this+IQ(-iq.amp, iq.angle)
    def shift(delta:Float)=IQ(amp, delta+angle)
  }
  object IQ{
    def fromIQ(I:Float, Q:Float)=IQ(Math.sqrt(I*I+Q*Q).asInstanceOf[Float], Math.atan2(Q, I).asInstanceOf[Float])
  }

  def pairMap[T3, T1, T2](arr1:Array[T1], arr2:Array[T2])(f:(T1,T2)=>T3)= {
    Array.range(0, arr1.length).map { i =>
      f(arr1(i), arr2(i))
    }
  }

  def printsig(sig:Array[Float]): Unit ={
    print(sig(0))
    for(i<-1 until sig.length)
      print("\t"+sig(i))
    println()
  }

}
