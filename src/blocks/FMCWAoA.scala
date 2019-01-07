package blocks

import config.AcousticProperty
import offline.AsciiOutput
import utils.pairMap

import scala.collection.mutable

class FMCWAoA {
  private val micConfig=AcousticProperty.VER2_CHANNELS_CONFIG
  private val micConfigWIdx=micConfig.zipWithIndex
  private val verticalPairs=micConfigWIdx.groupBy{case ((x,y), idx)=>y}.map{case (y, arr)=>(y, arr.sortBy{case ((x,_), _)=>x})}
  private val horizontalPairs=micConfigWIdx.groupBy{case ((x,y), idx)=>x}.map{case (x, arr)=>(x, arr.sortBy{case ((_,y), _)=>y})}

  private val QUARTER1=AcousticProperty.VER2_FMCW_WINDOWSIZE/4
  private val QUARTER2=AcousticProperty.VER2_FMCW_WINDOWSIZE/2
  private val QUARTER3=3*AcousticProperty.VER2_FMCW_WINDOWSIZE/4

  private val OFFSET_SMOOTH=0.8f


  //-pi to pi
  private def fitin2pi(f:Float)= {
    val raw2pi =
      if (f < 0) 2 * Math.PI.toFloat + (-f) % (2 * Math.PI.toFloat)
      else f % (2 * Math.PI.toFloat)
    if(raw2pi>Math.PI)
      raw2pi-2*Math.PI.toFloat
    else raw2pi
  }


  private class State{
    var velocity=0f //s/s
    var phaseOffset=0f
    var lastEndTm=0f
    var inited=false
    var totalv=0f
    var deltat=0f
    var lastEndPhase=0f
    var lastStartPhase=0f
  }

  private val historyt=mutable.Queue[Float]()
  private val MAXHISTORY=20

  private val states=Array.fill(micConfig.length)(new State())

  private var deltav=0f


  def reset(): Unit ={
    states.foreach{x=>
      x.velocity=0f
      x.phaseOffset=0f
      x.lastEndTm=0f
      x.deltat=0f
      x.totalv=0f
      x.inited=false
      x.lastEndPhase=0f
      x.lastStartPhase=0f
    }
    deltav=0f
    historyt.clear()
  }

  private def estimateFreqInitPhase(phases:Array[Float])= {
    val meanx = AcousticProperty.VER2_FMCW_WINDOWSIZE / 2
    var meany = 0f
    for (i <- QUARTER1 to QUARTER3) meany += phases(i) / (AcousticProperty.VER2_FMCW_WINDOWSIZE / 2 + 1)
    var b1 = 0f
    var b2 = 0f
    for (i <- QUARTER1 to QUARTER3) {
      b1 += (i - meanx) * (phases(i) - meany)
      b2 += (i - meanx) * (i - meanx)
    }
    val b = b1 / b2
    val a = meany - b * meanx
    val freq = b * AcousticProperty.VER2_FMCW_WINDOWSIZE / Math.PI.toFloat / 2
    (freq, a)
  }

  def estimateFreqGivenTm(tm:Float)={
    val F=AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART
    val T=AcousticProperty.VER2_FMCW_CHIRP_DURATION
    val freq=tm*AcousticProperty.VER2_FMCW_CHIRP_DURATION*0.75f*F/T
    freq
  }

  def estimatePhaseGivenTm(tm:Float, portion:Float)={
    val F=AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART
    val T=AcousticProperty.VER2_FMCW_CHIRP_DURATION
    val A=portion*3/2*Math.PI.toFloat*F+2*Math.PI.toFloat*AcousticProperty.VER2_FMCW_FSTART
    val C= Math.PI.toFloat*F/T
    val phi=C*tm*tm+A*tm
    phi
  }
  //phase at beginning
  def estimatePhaseGivenFreq(freq:Float, portion:Float)={
    val F=AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART
    val T=AcousticProperty.VER2_FMCW_CHIRP_DURATION
    val tm=freq/AcousticProperty.VER2_FMCW_CHIRP_DURATION/0.75f/F*T
    val A=portion*3/2*Math.PI.toFloat*F+2*Math.PI.toFloat*AcousticProperty.VER2_FMCW_FSTART
    val C= Math.PI.toFloat*F/T
    val phi=C*tm*tm+A*tm
    phi
  }



  // return delta, delta+phact=phest (mod 2pi)
  def phaseDiff(phest:Float, phact:Float)={
    val mul=Math.floor((phest-phact)/Math.PI/2).toFloat
    phest-(mul*Math.PI.toFloat*2+phact)
  }

  def estimateTmGivenPhase(ph:Float, portion:Float)= {
    val F = AcousticProperty.VER2_FMCW_FEND - AcousticProperty.VER2_FMCW_FSTART
    val T = AcousticProperty.VER2_FMCW_CHIRP_DURATION
    val A = portion * 3 / 2 * Math.PI.toFloat * F + 2 * Math.PI.toFloat * AcousticProperty.VER2_FMCW_FSTART
    val C = Math.PI.toFloat * F / T
    val tm = (-A + Math.sqrt(A * A + 4 * C * ph).toFloat) / C / 2
    tm
  }

  //delta+relPh~absPh
  def get2piDelta(relPh:Float, absPh:Float, offset:Float)={
    val diff=absPh-relPh-offset
    val mul=Math.round(diff/2/Math.PI.toFloat)
    val newoffset=absPh-relPh-mul*2*Math.PI.toFloat
    (newoffset, mul*2*Math.PI.toFloat)
  }


  def getDistFromTm(tm:Float)=tm*AcousticProperty.SOUND_SPEED*1000

  def triangulate(tms:Array[Float])={
    def mid(t1:Double, t2:Double, distt:Double)=Math.sqrt((t1*t1+t2*t2)/2-distt*distt/4)
    val xs=for ((y, arr)<-verticalPairs) yield{
      val ((x1, _), idx1)=arr(0)
      val ((x2, _), idx2)=arr(1)
      val dist=Math.abs(x1-x2) //in m
      val distt=dist/AcousticProperty.SOUND_SPEED

      val t1=tms(idx1)+states(idx1).deltat
      val t2=tms(idx2)+states(idx2).deltat

      ((t1*t1-t2*t2)/distt/2, mid(t1,t2,distt), distt)
    }


    val ys=for ((x, arr)<-horizontalPairs) yield{
      val ((_, y1), idx1)=arr(0)
      val ((_, y2), idx2)=arr(1)
      val dist=Math.abs(y1-y2) //in m
      val distt=dist/AcousticProperty.SOUND_SPEED

      val t1=tms(idx1)+states(idx1).deltat
      val t2=tms(idx2)+states(idx2).deltat

      ((t1*t1-t2*t2)/distt/2, mid(t1,t2,distt), distt)
    }
    val x=xs.map(_._1).sum/xs.size
    val y=ys.map(_._1).sum/ys.size
    val (zs1, zs2)=(xs.map(_._2), ys.map(_._2))

    val (distt1, distt2)=(xs.map(_._3).sum/xs.size, ys.map(_._3).sum/ys.size)
    val z1=mid(zs1.head, zs1.last, distt2)
    val z2=mid(zs2.head, zs2.last, distt1)

    (x, y, ((z1+z2)/2).asInstanceOf[Float])
  }

  def getAoA(tms:Array[Float]) ={
    val xangles=for ((y, arr)<-verticalPairs) yield{
      val ((x1, _), idx1)=arr(0)
      val ((x2, _), idx2)=arr(1)
      val dist=Math.abs(x1-x2) //in m

      val t1=tms(idx1)+states(idx1).deltat
      val t2=tms(idx2)+states(idx2).deltat

      val rdist=(t1-t2)*AcousticProperty.SOUND_SPEED // in m
      val angle=Math.asin(rdist/dist)
      angle
    }


    val yangles=for ((x, arr)<-horizontalPairs) yield{
      val ((_, y1), idx1)=arr(0)
      val ((_, y2), idx2)=arr(1)
      val dist=Math.abs(y1-y2) //in m

      val t1=tms(idx1)+states(idx1).deltat
      val t2=tms(idx2)+states(idx2).deltat

      val rdist=(t1-t2)*AcousticProperty.SOUND_SPEED // in m
      val angle=Math.asin(rdist/dist)
      angle
    }

    (xangles.sum/xangles.size, yangles.sum/yangles.size)
  }

  def resetAngleAndSpeed(dist:Float): Unit ={
    //first calculate speed
    var b1=0f
    var b2=0f
    val meanx=(historyt.length-1)/2f
    val meany=historyt.sum/historyt.length
    for((t, i)<-historyt.zipWithIndex){
      b1+=(i-meanx)*(t-meany)
      b2+=(i-meanx)*(i-meanx)
    }
    this.deltav= -b1/b2

    //then calibrate t
    val t= -dist/AcousticProperty.SOUND_SPEED
    for(state<-states) {
      state.deltat = t - state.lastEndTm
    }
    println("calibrated")
  }


  def getTm(phases:Array[Array[Float]])= {

    def getAvgTm(state:State, phDelta:Float, phase:Array[Float])={
      val N=0
      val tms=for(i<-QUARTER3-N to QUARTER3) yield {
        estimateTmGivenPhase(phDelta + state.phaseOffset + phase(i), (i-QUARTER2)*0.25f/(QUARTER3-QUARTER2)+0.5f)
      }
      tms.sum/tms.size
    }

    //get the accurate distance estimate
    val tms = phases.zip(states).map { case (phase, state) =>
      if (!state.inited) {
        val (estFreq, initPhase) = estimateFreqInitPhase(phase)
        val estPhase = estimatePhaseGivenFreq(estFreq, 0)
        val phDiff = phaseDiff(estPhase, initPhase)
        state.inited = true
        state.phaseOffset = phDiff


        val phLast = phase(QUARTER3) - initPhase + estPhase
        val tmLast = estimateTmGivenPhase(phLast, 0.75f)

        state.lastEndTm = tmLast
        state.lastStartPhase=phase(QUARTER1)-initPhase+estPhase
        state.lastEndPhase=phase(QUARTER3)-initPhase+estPhase

        tmLast

      } else {
        //first estimate start phase
        val estTmStart = state.lastEndTm + state.velocity *
          (AcousticProperty.VER2_FMCW_CHIRP_DURATION * 7 / 16 + AcousticProperty.VER2_FMCW_GUARD_DURATION)
        val estPhStart = estimatePhaseGivenTm(estTmStart, 0.25f)
        val actPhStart = phase(QUARTER1)
        val (newoffset, actPhDelta) = get2piDelta(actPhStart, estPhStart, state.phaseOffset)
        state.phaseOffset = state.phaseOffset * OFFSET_SMOOTH + newoffset * (1 - OFFSET_SMOOTH)

        val newStartPhase=actPhDelta+state.phaseOffset+phase(QUARTER1)
        val newEndPhase=actPhDelta+state.phaseOffset+phase(QUARTER3)
        val diffstart=newStartPhase-state.lastStartPhase
        val diffend=newEndPhase-state.lastEndPhase

        if(Math.abs(diffstart-diffend)>1.5*Math.PI){
          //error happens, need interpolate
          println("interpolated", newStartPhase, state.lastStartPhase, newEndPhase, state.lastEndPhase)
          phase(QUARTER3)+=(Math.round((diffstart-diffend)/2/Math.PI)*2*Math.PI).toFloat
          phase(QUARTER2)=(phase(QUARTER3)+phase(QUARTER1))/2

        }
        state.lastEndPhase=actPhDelta+state.phaseOffset+phase(QUARTER3)
        state.lastStartPhase=actPhDelta+state.phaseOffset+phase(QUARTER1)

        val estTmMid = estimateTmGivenPhase(actPhDelta + state.phaseOffset + phase(QUARTER2), 0.5f)
        val estTmLast = estimateTmGivenPhase(actPhDelta + state.phaseOffset + phase(QUARTER3), 0.75f)
        val estVel = (estTmLast - state.lastEndTm) / AcousticProperty.VER2_FMCW_CYCLE_DURATION

        state.velocity = estVel
        state.lastEndTm = estTmLast

        getAvgTm(state, actPhDelta, phase)
      }
    }



    historyt += tms.sum / tms.length
    if (historyt.length > MAXHISTORY) historyt.dequeue()

    states.foreach(_.totalv += deltav)

    tms

  }
  def AoA(tms:Array[Float])={
    //get aoa
    val aoa=getAoA(tms)
    val tm=states.zip(tms).map{
      case (state, t)=>state.deltat+t
    }.sum/tms.length

    (tm, aoa._1, aoa._2)

  }
}
