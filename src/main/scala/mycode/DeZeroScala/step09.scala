package mycode.DeZeroScala
import scala.math.exp

object step09 extends App{
  case class Variable(val data:List[Double]){
    var grad = List[Double]()
    var creator:Function = null

    def backward():Unit={
      val f = creator
      if(grad == List[Double]()) grad = List.fill(data.length)(1.0)
      if(f != null){
        val x = f.input
        x.grad = f.backward(grad)
        x.backward()
      }
    }

    def +(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x+y)}
      Variable(output)
    }
    def +(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){output = output :+ (x+input)}
      Variable(output)
    }
    def -(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x-y)}
      Variable(output)
    }
    def -(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){output = output :+ (x-input)}
      Variable(output)
    }
    def *(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x*y)}
      Variable(output)
    }
    def *(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){output = output :+ (x*input)}
      Variable(output)
    }
    def /(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x/y)}
      Variable(output)
    }
    def /(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){output = output :+ (x/input)}
      Variable(output)
    }
    def Exp():Variable={
      var output = List[Double]()
      for(x <- data){output = output :+ exp(x)}
      Variable(output)
    }
    override def toString():String="[" + data.toString + "]"
  }
  class Function(val input:Variable){
    val x = input.data
    val y = forward(x)
    val output = Variable(y)
    def forward(in_data:List[Double]):List[Double] = throw new NoSuchElementException
    def backward(grad:List[Double]):List[Double] = ??? 
  }
  object Function {
    def apply(input:Variable) ={
      val f = new Function(input)
      f.output.creator = f
      f.output
    }
  }

  class Square(override val input:Variable) extends Function(input) {
    override def forward(in_data:List[Double]):List[Double]={
      val output = Variable(in_data) * Variable(in_data)
      output.data
    }
    override def backward(grad:List[Double]):List[Double]={
      val output = (input * 2) * Variable(grad)
      output.data
    }
  }
  object Square {
    def apply(input:Variable):Variable={
      val n = new Square(input)
      n.output.creator = n
      n.output
    }
  }
  class Exp(override val input:Variable) extends Function(input){
    override def forward(in_data:List[Double]):List[Double]={
      val output = Variable(in_data).Exp()
      output.data
    }
    override def backward(grad:List[Double]):List[Double]={
      val output = input.Exp() * Variable(grad)
      output.data
    }
  }
  object Exp{
    def apply(input:Variable):Variable={
      val n = new Exp(input)
      n.output.creator = n
      n.output
    }
  }

  val data = List(0.5,1,1.5,2.0)
  val x = Variable(data)
  println("Input x:" + x.toString)
  println("forward.....")
  val a = Square(x)
  val b = Exp(a)
  val y = Square(b)
  println("Output y:" + y.toString)
  println("backward.....")
  //y.grad = List.fill(4)(1.0)
  y.backward()
  println("dy/dx:" + x.grad.toString)
  def anotherF(input:Variable):Variable={
    val a = Square(input)
    val b = Exp(a)
    val y = Square(b)
    y
  }
  def another_diff(f:Variable=>Variable,input:Variable,eps:Double=1e-4):Variable={
    
    val x0 = input - eps
    val x1 = input + eps
    val y0 = f(x0)
    val y1 = f(x1)
    val output = (y1 - y0) / (2 * eps)
    output
  }
  val anotherDy = another_diff(anotherF,x).data
  println("anotherDy:" + anotherDy.toString)


}
