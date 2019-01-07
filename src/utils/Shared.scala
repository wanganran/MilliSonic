package utils

case class Shared[T]() {
  @volatile var data:Option[T]=None

  def update(t:T): Unit ={
    this.data=Some(t)
  }

  def getOrElse(default:T)=data.getOrElse(default)
}
