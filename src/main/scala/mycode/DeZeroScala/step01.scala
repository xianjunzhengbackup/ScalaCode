package mycode.DeZeroScala

object step01 extends App{

  //val l = List(11.0,2.1,3.4444,4.56789012)
  //println(l.toString)
  case class Variable(val data:List[Double]){

  }
  val data = List(1.0)
  val x = Variable(data)
  println(x.data)
}
