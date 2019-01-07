package utils


class FIRFilter(val sampleRate:Int) {
  trait Filter{def apply(sig:Array[Float], lastSig:Array[Float], res:Array[Float]):Unit}

  def getHammingWindow(wind:Array[Float]){
    assert(wind.size%2==1)
    for(i<-wind.indices){
      wind(i)=(0.53836-0.46164*Math.cos(2*Math.PI*i/(wind.size-1))).asInstanceOf[Float]
    }
  }

  def getFIRFilter(halfWind:Int, f1:Int, f2:Int):Filter= {
    val mask = new Array[Float](halfWind * 2 + 1)
    getHammingWindow(mask)
    for (i <- 0 until halfWind * 2 + 1) {
      val n = halfWind - i
      mask(i) = mask(i) * (Math.sin(2 * Math.PI * n * f2 / sampleRate) / n / Math.PI -
        Math.sin(2 * Math.PI * n * f1 / sampleRate) / n / Math.PI).asInstanceOf[Float]
    }
    mask(halfWind) = 2 * (f2 - f1).asInstanceOf[Float] / sampleRate
    val sum = mask.sum
    for (i <- 0 until halfWind * 2 + 1) mask(i) /= sum

    new Filter {
      def apply(sig: Array[Float], lastSig: Array[Float], res: Array[Float]) {
        FIR(sig, lastSig, sig.length, mask, 2 * halfWind + 1, res)
        //remove dc
        val mean=res.sum/res.length
        for(i<-res.indices)res(i)-=mean
      }
    }
  }

  //translate from c++. perf critical. use while loop instead of for
  private def FIR(sig:Array[Float],
    oldsig:Array[Float],
    sigLen:Int,
    mask:Array[Float],
    maskLen:Int,
    res:Array[Float]): Unit = {
    //println(sigLen)
    //println(maskLen)
    val mid = maskLen / 2
    var i = -mid
    while (i < mid) {
      var tot = 0f
      var j = 0
      while (j < maskLen - i - mid - 1) {
        tot += mask(j) * oldsig(sigLen + i - mid + j)
        j += 1
      }
      j = maskLen - i - mid - 1
      while (j < maskLen) {
        tot += mask(j) * sig(j - maskLen + i + mid + 1)
        j += 1
      }
      res(i + mid) = tot

      i += 1
    }
    i = mid
    while (i < sigLen - mid) {
      var tot = 0f
      var j = 0
      while (j < maskLen) {
        tot += sig(i - mid + j) * mask(j)
        j += 1
      }
      res(i + mid) = tot

      i += 1
    }
  }
}