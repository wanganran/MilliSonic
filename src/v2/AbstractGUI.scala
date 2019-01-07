package v2

/**
  * Created by wanganran on 11/19/18.
  */
trait AbstractGUI {
  var callback: (Symbol => Unit)
  def update(sample:Any, draw:Boolean)
}
