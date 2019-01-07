package blocks

import blocks.FMCWFilter.{PhaseResult, StartPoint}
import config.AcousticProperty
import utils.Conv
import utils.argmax
import utils.argmin
import utils.getAngle

object FMCWFilter{
  abstract class FMCWResult
  case class StartPoint(offset:Int) extends FMCWResult
  case class PhaseResult(phases:Array[Float]) extends  FMCWResult
}

class FMCWFilter(id:Int) {

  private val WINDOWSIZE=AcousticProperty.VER2_FMCW_WINDOWSIZE
  private val WIDTH=WINDOWSIZE/4-1
  private val GAPBEGIN=AcousticProperty.VER2_FMCW_CHIRP_DURATION_SAMPLE/5 //0.008s
  private val convo=new Conv(WINDOWSIZE)
  private val convFFT=new Conv(AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE)

  private val freqMin=(AcousticProperty.VER2_FMCW_FSTART-2000)*AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE/AcousticProperty.VER2_SR
  private val freqMax=(AcousticProperty.VER2_FMCW_FEND+2000)*AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE/AcousticProperty.VER2_SR

  private val upchirpTemplateI=Array.range(0, AcousticProperty.VER2_FMCW_CHIRP_DURATION_SAMPLE).map{i=>
    val t=i.toFloat/AcousticProperty.VER2_SR
    val A=(AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART)/AcousticProperty.VER2_FMCW_CHIRP_DURATION
    Math.cos(2*Math.PI*(AcousticProperty.VER2_FMCW_FSTART*t+A/2*t*t)).toFloat
  }

  private val upchirpTemplateQ=Array.range(0, AcousticProperty.VER2_FMCW_CHIRP_DURATION_SAMPLE).map{i=>
    val t=i.toFloat/AcousticProperty.VER2_SR
    val A=(AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART)/AcousticProperty.VER2_FMCW_CHIRP_DURATION
    -Math.cos(2*Math.PI*(AcousticProperty.VER2_FMCW_FSTART*t+A/2*t*t)-Math.PI/2).toFloat
  }


  private val downchirpTemplateI=Array.range(0, AcousticProperty.VER2_FMCW_CHIRP_DURATION_SAMPLE).map{i=>
    val t=i.toFloat/AcousticProperty.VER2_SR
    val A=(AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART)/AcousticProperty.VER2_FMCW_CHIRP_DURATION
    Math.cos(2*Math.PI*(AcousticProperty.VER2_FMCW_FEND*t-A/2*t*t)).toFloat
  }

  private val downchirpTemplateQ=Array.range(0, AcousticProperty.VER2_FMCW_CHIRP_DURATION_SAMPLE).map{i=>
    val t=i.toFloat/AcousticProperty.VER2_SR
    val A=(AcousticProperty.VER2_FMCW_FEND-AcousticProperty.VER2_FMCW_FSTART)/AcousticProperty.VER2_FMCW_CHIRP_DURATION
    Math.cos(2*Math.PI*(AcousticProperty.VER2_FMCW_FEND*t-A/2*t*t)-Math.PI/2).toFloat
  }

  //inputs are all complex
  private def conv(sig:Array[Float], kernel:Array[Float])={
    convo.fftComplexKernel(kernel)
    convo.convComplex(sig, kernel)
  }

  def adjustOffset(ind:Int)={
    offset=Some(ind)
  }
  def getOffset=offset

  def reset()={
    offset=None
    lastPeak=None
  }

  private var lastPeak:Option[Float]=None

  def updatePeak(peakFreq:Float): Unit ={
    lastPeak=Some(peakFreq)
  }

  private var offset:Option[Int]=None

  private val lastRealSig=new Array[Float](AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE)
  private val cycleBuffer=new Array[Float](AcousticProperty.VER2_FMCW_CYCLE_DURATION_SAMPLE)
  private val decodeBuffer=new Array[Float](WINDOWSIZE*2)

  // obsolete
  private def buildWindow(kernel:Array[Float]): Unit ={
    for(i<-0 until WIDTH) {
      val w = 0.53836f - 0.46164f * Math.cos(2 * Math.PI * i / WIDTH).toFloat
      kernel(i * 2) = w
      kernel(i * 2 + 1) = w
      if (i != 0) {
        kernel((WINDOWSIZE - i) * 2) = w
        kernel((WINDOWSIZE - i) * 2 + 1) = w
      }
    }
  }

  private def buildKernel(f:Float)= {
    val kernel = new Array[Float](WINDOWSIZE * 2)

    for (i <- 0 until WIDTH) {
      kernel(i * 2) = Math.cos(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      kernel(i * 2 + 1) = -Math.sin(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      if (i != 0) {
        kernel((WINDOWSIZE - i) * 2) = Math.cos(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
        kernel((WINDOWSIZE - i) * 2 + 1) = -Math.sin(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      }
    }
    kernel
  }


  //return (raw_phases, raw_freq, distance (in s), velocity (relative to speed of sound))
  private def doFMCWFilter(chirpI:Array[Float], chirpQ:Array[Float], realSig:Array[Float], begin:Int)= {
    convFFT.realBpf(realSig, freqMin, freqMax)

    for (i <- 0 until WINDOWSIZE) {
      decodeBuffer(i*2) = chirpI(i) * realSig(i+begin)
      decodeBuffer(i*2+1) = chirpQ(i) * realSig(i+begin)
    }
    if (lastPeak.isEmpty) {
      val copyBuffer = new Array[Float](decodeBuffer.length)
      Array.copy(decodeBuffer, 0, copyBuffer, 0, decodeBuffer.length)
      val freqs = convo.fftComplexKernel(copyBuffer)
      for (i <- 1 until WINDOWSIZE)
        freqs(i) = freqs(i*2) * freqs(i*2) + freqs(i*2+1) * freqs(i*2+1)
      val freqMax = argmax(freqs, WINDOWSIZE)
      lastPeak = Some(freqMax)
    }
    val kernel = buildKernel(lastPeak.get)
    convo.fftComplexKernel(kernel)


    convo.convComplex(decodeBuffer, kernel)

    var totalDiff = 0f
    var lastRawphase=0f
    val phaseBuffer=new Array[Float](WINDOWSIZE)

    //get lowest and check phase
    val LOWNEIGHBOR=WINDOWSIZE/20
    val expectedPhaseDiff=lastPeak.get *Math.PI*2*LOWNEIGHBOR*2/WINDOWSIZE

    for(i<-WIDTH until WINDOWSIZE-WIDTH){
      phaseBuffer(i)=decodeBuffer(i*2)*decodeBuffer(i*2)+decodeBuffer(i*2+1)*decodeBuffer(i*2+1)
    }

    val lowid=argmin(phaseBuffer, WIDTH+LOWNEIGHBOR, WINDOWSIZE-WIDTH*2-LOWNEIGHBOR*2)



    for (i <- WIDTH until WINDOWSIZE - WIDTH) {
      val rawPhase = getAngle(decodeBuffer(i*2), decodeBuffer(i*2+1))
      if (i != WIDTH) {
        val rawdiff = rawPhase-lastRawphase
        if (rawdiff > Math.PI) {
          val actdiff = rawdiff - 2 * Math.PI.toFloat
          totalDiff += rawdiff - 2 * Math.PI.toFloat
          phaseBuffer(i) = phaseBuffer(i - 1) + actdiff
        }
        else if (rawdiff < -Math.PI) {
          val actdiff = rawdiff + 2 * Math.PI.toFloat
          totalDiff += rawdiff + 2 * Math.PI.toFloat
          phaseBuffer(i) = phaseBuffer(i - 1) + actdiff
        }
        else {
          totalDiff += rawdiff
          phaseBuffer(i) = phaseBuffer(i - 1) + rawdiff
        }
        lastRawphase=rawPhase
      } else {
        lastRawphase=rawPhase
        phaseBuffer(i)=rawPhase
      }
    }

    val phdiff=phaseBuffer(lowid+LOWNEIGHBOR)-phaseBuffer(lowid-LOWNEIGHBOR)
    val diffdist=expectedPhaseDiff-phdiff

    val peak=totalDiff/(WINDOWSIZE-2*WIDTH-1)*WINDOWSIZE/Math.PI.toFloat/2
    (phaseBuffer, peak)
  }

  var time=System.currentTimeMillis()
  //realSig is disposable
  //return (phase diff array, avg freq, dist, vel)
  def input(realSig:Array[Float])={
    offset match {
      case None=>
        val start=convFFT.corrReal(realSig, upchirpTemplateI)
        offset=Some(start)
        Array.copy(realSig, 0, lastRealSig, 0, realSig.length)
        //println(start)
        StartPoint(start)
      case Some(start)=>
        Array.copy(lastRealSig, start, cycleBuffer, 0, lastRealSig.length-start)
        Array.copy(realSig, 0, cycleBuffer, lastRealSig.length-start, start)
        val (phases, freqUp)=doFMCWFilter(upchirpTemplateI, upchirpTemplateQ, cycleBuffer, GAPBEGIN)

        lastPeak=Some(freqUp)
        //println(freqUp)
        Array.copy(realSig, 0, lastRealSig, 0, realSig.length)
        PhaseResult(phases)
    }
  }
}
