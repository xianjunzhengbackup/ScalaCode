package mycode.DeZeroScala
import scala.math.exp

object step06 extends App{
  case class Variable(val data:List[Double]){
    var grad = List[Double]()
    var temp_grad = List[Double]()
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
      val l = input.data.length
      val p = List.fill(l)(1.0)
      n.output.temp_grad = n.backward(p)
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
      val l = input.data.length
      val p = List.fill(l)(1.0)
      n.output.temp_grad = n.backward(p)
      n.output
    }
  }

  def numerical_diff(f:Function,x:Variable,eps:Double=1e-4):Variable={
    val x0_list = x - eps
    val x1_list = x + eps
    val y0_list = f.forward(x0_list.data)
    val y1_list = f.forward(x1_list.data)
    val output = (Variable(y1_list) - Variable(y0_list)) / (2 * eps)
   output
  }
  val data = List(0.5,1,1.5,2.0)
  val x = Variable(data)
  println("Input x:" + x.toString)
  println("forward.....")
  val A = new Square(x)
  val a = Square(x)
  val b = Exp(a)
  val y = Square(b)
  println("Output y:" + y.toString)
  val dy = numerical_diff(A,x)
  println("dy/dx:" + dy.toString)
  println("backward.....")
  y.grad = List[Double](1.0,1.0,1.0,1.0)
  b.grad = (Variable(y.temp_grad) * Variable(y.grad)).data
  a.grad = (Variable(b.grad) * Variable(b.temp_grad)).data
  x.grad = (Variable(a.grad) * Variable(a.temp_grad)).data
  println("dy/dx:"+x.grad.toString)


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
