package mycode.DeZeroScala
import scala.math.exp

object step03 extends App{
  case class Variable(val data:List[Double])
  class Function(val input:Variable){
    val x = input.data
    val y = forward(x)
    val output = Variable(y)
    def forward(in_data:List[Double]):List[Double] = ???
  }
  object Function {
    def apply(input:Variable) ={
      val f = new Function(input)
      f.output
    }
  }

  class Square(override val input:Variable) extends Function(input) {
    override def forward(in_data:List[Double]):List[Double]={
      var container=List[Double]()
      in_data.foreach(element=>{
        val temp = element * element
        container = container :+ temp
      })
      container
    }
  }
  object Square {
    def apply(input:Variable):Variable={
      val n = new Square(input)
      n.output
    }
  }
  class Exp(override val input:Variable) extends Function(input){
    override def forward(in_data:List[Double]):List[Double]={
      var container = List[Double]()
      in_data.foreach(element=>{
        val temp=exp(element)
        container = container :+ temp
      })
      container
    }
  }
  object Exp{
    def apply(input:Variable):Variable={
      val n = new Exp(input)
      n.output
    }
  }
  val data = List(0.5,1,1.5,2.0)
  val x = Variable(data)
  println(x)
  val a = Square(x)
  val b = Exp(a)
  val y = Square(b)
  println(y.data)
}
