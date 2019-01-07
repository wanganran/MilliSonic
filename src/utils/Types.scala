package utils

object Types {
  type PeakSNRPair=(Float, Float)
  type RawSignal=Array[Float]
  type Phase=Array[Float]
  type Orientation=(Float,Float)

  //second per second. Positive means distance will be farther
  type Drift=Float
  type SNR=Float
}
