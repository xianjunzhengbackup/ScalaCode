package mycode.DeZeroScala
import scala.math.exp

object step04 extends App{
  case class Variable(val data:List[Double]){
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
    override def toString():String="[" + data.toString + "]"
  }
  class Function(val input:Variable){
    val x = input.data
    val y = forward(x)
    val output = Variable(y)
    def forward(in_data:List[Double]):List[Double] = throw new NoSuchElementException
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

  def numerical_diff(f:Function,x:Variable,eps:Double=1e-4):Variable={
    val x0_list = x - eps
    val x1_list = x + eps
    //x.data.foreach(element=>{
    //  val x0 = element - eps
    //  val x1 = element + eps
    //  x0_list = x0_list :+ x0
    //  x1_list = x1_list :+ x1
    //})
    val y0_list = f.forward(x0_list.data)
    val y1_list = f.forward(x1_list.data)
    /*(y0_list zip y1_list).foreach(element2 =>{
      val y0 = element2(0)
      val y1 = element2(1)
      val temp = (y1 - y0) / (2*eps)
      output = output :+ temp
    })*/
   //for{(y0,y1) <- y0_list zip y1_list}{
   // val temp = (y1 - y0) / (2 * eps)
   // output = output :+ temp
   //}
   val output = (Variable(y1_list) - Variable(y0_list)) / (2 * eps)
   output
  }
  val data = List(0.5,1,1.5,2.0)
  val x = Variable(data)
  println(x)
  val A = new Square(x)
  val a = Square(x)
  val b = Exp(a)
  val y = Square(b)
  println(y.data)
  val dy = numerical_diff(A,x)
  println("dy:" + dy.toString)

  def anotherF(input:Variable):Variable={
    val a = Square(input)
    val b = Exp(a)
    val y = Square(b)
    y
  }
  def another_diff(f:Variable=>Variable,input:Variable,eps:Double=1e-4):Variable={
    
    //var output = List[Double]()
    //input.data.foreach(element=>{
    //  val x0 = element - eps
    //  val x1 = element + eps
    //  val y0 = f(Variable(List(x0)))
    //  val y1 = f(Variable(List(x1)))
    //  val temp = (y1.data(0) - y0.data(0)) / (2 * eps)
    //  output = output:+ temp
    //})

    //val o = Variable(output)
    //o
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
