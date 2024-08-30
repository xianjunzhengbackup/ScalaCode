package mycode.DeZeroScala
import scala.math.exp

object step13 extends App{
  case class Variable(val data:List[Double]){
    var grad = List[Double]()
    var creator:Function = null

    def backward():Unit={
      val f = creator
      if(grad == List[Double]()) grad = List.fill(data.length)(1.0)
      if(f != null){
        val xs = f.input
        if(xs.size == 1) {
          if(xs(0).grad == List[Double]()) xs(0).grad = List.fill(data.length)(0.0)
          println("input is:"+xs)
          println("old grad is:"+xs(0).grad.toString)
          if(xs(0).creator == null) xs(0).grad = (Variable(f.backward(grad)) + Variable(xs(0).grad)).data
          else xs(0).grad = f.backward(grad)
          println("new grad is:"+xs(0).grad.toString)
          xs(0).backward()
        } else{
          for(x <- xs){
            if(x.grad == List[Double]()) x.grad = List.fill(data.length)(0.0)
            println("input is:"+x)
            println("old grad is:"+x.grad.toString)
            if(x.creator == null) x.grad = (Variable(f.backward(grad,x.data)) + Variable(x.grad)).data
            else x.grad = f.backward(grad,x.data)
            println("new grad is:"+x.grad.toString)
            x.backward()
          }
        }
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
  class Function(val input:Variable*){
    //val x = input.data
    val y = forward()
    val output = Variable(y)
    def forward():List[Double] = throw new NoSuchElementException
    def backward(grad:List[Double]):List[Double] = ??? 
    def backward(grad:List[Double],x:List[Double]):List[Double] = ???
  }
  object Function {
    def apply(input:Variable*) ={
      val f = new Function(input:_*)
      f.output.creator = f
      f.output
    }
  }

  class Square(override val input:Variable*) extends Function(input:_*) {
    override def forward():List[Double]={
      val in_data = input(0)
      val output = in_data * in_data
      output.data
    }
    override def backward(grad:List[Double]):List[Double]={
      val output = (input(0) * 2) * Variable(grad)
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
  class Exp(override val input:Variable*) extends Function(input:_*){
    override def forward():List[Double]={
      val output = input(0).Exp()
      output.data
    }
    override def backward(grad:List[Double]):List[Double]={
      val output = (input(0).Exp()) * Variable(grad)
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
  class Add(override val input:Variable*) extends Function(input:_*){
    override def forward():List[Double]={
      var out=List[Double]()
      def small_forward(x1:List[Double],x2:List[Double]):List[Double]=(Variable(x1) + Variable(x2)).data
      for(x <- input) {if(out==List[Double]()) out=x.data; else out = small_forward(out,x.data)}
      out
    }
    override def backward(grad:List[Double],x:List[Double])={
      grad
    }
  }
  object Add{
    def apply(inputs:Variable*):Variable={
      val n = new Add(inputs:_*)
      n.output.creator = n
      n.output
    }
  }
  class Mul(override val input:Variable*) extends Function(input:_*){
    override def forward():List[Double]={
      var out=List[Double]()
      def small_forward(x1:List[Double],x2:List[Double]):List[Double]=(Variable(x1) *  Variable(x2)).data
      for(x <- input) {if(out==List[Double]()) out=x.data; else out = small_forward(out,x.data)}
      out
    }
    override def backward(grad:List[Double],x:List[Double])={
      val out = (Variable(grad) * output) / Variable(x)
      out.data
    }
  }
  object Mul{
    def apply(inputs:Variable*):Variable={
      val n = new Mul(inputs:_*)
      n.output.creator = n
      n.output
    }
  }

  val x = Variable(List(0.5,1,1.5,2.0))
  val y = Variable(List(1.5,1,1.5,2.0))
  //val z = Variable(List(2.5,1,1.5,2.0))

  //val xyz = Mul(x,y,z)
  //println("xyz;"+xyz.toString)
  //xyz.backward()
  //println("d(xyz)/dx:",x.grad.toString)
  //println("d(xyz)/dy:",y.grad.toString)
  //println("d(xyz)/dz:",z.grad.toString)

  val a = Square(x)
  val c = Mul(x,y)
  val b = Square(y)

  val d = Mul(x,y)
  val e = Exp(d)
  val f = Mul(d,y)
  val g = Exp(f)
  val m = Add(g,e,b,a)
  println(m.data)
  m.backward()
  println("Backward....")
  println("x's grad:"+x.grad.toString)
  println("y's grad:"+y.grad.toString)


  //println("z's grad:"+z.grad.toString)
  //println("a(a=x * x)'s grad:"+a.grad.toString)
  //println("d(e^a)'s grad:"+d.grad.toString)

  //def anotherF(input:Variable*):Variable={
  //  var sum = Variable(List.fill(input(0).data.length)(0.0))
  //  for(in <- input) sum = sum + (in * in).Exp()
  //  sum
  //}
  //def another_diff(f:Variable:_*=>Variable,input:Variable,eps:Double=1e-4):Variable={
  //  
  //  val x0 = input - eps
  //  val x1 = input + eps
  //  val y0 = f(x0)
  //  val y1 = f(x1)
  //  val output = (y1 - y0) / (2 * eps)
  //  output
  //}
  //val anotherDy = another_diff(anotherF,x).data
  //println("anotherDy:" + anotherDy.toString)


}
