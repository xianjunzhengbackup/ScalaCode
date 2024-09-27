package mycode
package mycode.DeZeroScala

import java.io._
import scala.math._
import scala.sys.process._

object step29 extends App{
  val rand = new scala.util.Random
  def Id =(System.currentTimeMillis+rand.nextInt()).toString
  case class Variable(var data:List[Double],var name:String="",var ID:String=null,val isConstant:Boolean=false){
    var grad:Variable = null
    var creator:Function = null
  
    def clear_grad():Unit={grad = Variable(List[Double]())}
    def backward():Unit={
      val f = creator
      val empty_grad = grad == null
      val reach_first_layer = f == null
      if(empty_grad) grad = Variable(List.fill(data.length)(1.0),isConstant = true) //this is output layer,adding grad
      if(!reach_first_layer){
        val xs = f.input
        val single_input = xs.size == 1
        if(single_input) {
          //Inintialization for grad
          if(xs(0).grad.data == List[Double]()) xs(0).grad = Variable(List.fill(data.length)(0.0))
          //println("input is:"+xs)
          //println("old grad is:"+xs(0).grad.toString)
        
          //Reach the first layer,grad need accumulated.Otherwise just copy
          if(xs(0).creator == null) xs(0).grad = f.backward(grad) +++ xs(0).grad
          else xs(0).grad = f.backward(grad)
          //println("new grad is:"+xs(0).grad.toString)
          xs(0).backward()
        } else{
          for(x <- xs){
            if(!x.isConstant){
              //Inintialization for grad
              if (x.grad == null) x.grad = Variable(List.fill(data.length)(0.0))
              //println("input is:"+x)
              //println("old grad is:"+x.grad.toString)

              //Reach the first layer, grad need accumulated. Otherwise just copy
              if (x.creator == null) x.grad = f.backward(grad, x) +++ x.grad
              else x.grad = f.backward(grad, x)
              //println("new grad is:"+x.grad.toString)
              x.backward()
            }
          }
        }
      }
    }

    def +++(input:Variable):Variable={
      val f = Add("",this,input)
      f
    }
    def +++(input:Double):Variable={
      val f = Add("",this,Variable(List.fill(data.length)(input),isConstant = true))
      f
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
    def ---(input:Variable):Variable={
      val f = Sub("",this,input)
      f
    }
    def ---(input:Double):Variable={
      val f = Sub("",this,Variable(List.fill(data.length)(input),isConstant = true))
      f
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
    def ***(input:Variable):Variable={
      val f = Mul("",this,input)
      f
    }
    def ***(input:Double):Variable={
      val f = Mul("",this,Variable(List.fill(data.length)(input),isConstant = true))
      f
    }
    def *(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){
        val tmp:Double = x*y 
        output = output :+ tmp
      }
      Variable(output)
    }
    def *(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){
        val tmp = x * input
        output = output :+ tmp}
      Variable(output)
    }
    def **(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){
        val tmp = pow(x,input)
        output = output :+ tmp}
      Variable(output)
    }
    def **(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){
        val tmp = pow(x,y)
        output = output :+ tmp}
      Variable(output)
    }
    def /(input:Double):Variable={
      var output = List[Double]()
      for(x <- data){
        val tmp = x/ input
        if(tmp.isNaN) output = output :+ 0.0
        else output = output :+ (x/input)}
      Variable(output)
    }
    def /(input:Variable):Variable={
      var output = List[Double]()
      for((x,y) <- data zip input.data){
        val tmp = x/ y
        if(tmp.isNaN) output = output :+ 0.0
        else output = output :+ (x/y)}
      Variable(output)
    }
    def \\\(input:Variable):Variable={
      val f = Div("",this,input)
      f
    }
    def \\\(input:Double):Variable={
      val f = Div("",this,Variable(List.fill(data.length)(input),isConstant = true))
      f
    }
    def ^^^(input:Variable):Variable={
      val f = Pow("",this,input)
      f
    }
    def ^^^(input:Double):Variable={
      val f = Pow("",this,Variable(List.fill(data.length)(input),isConstant = true))
      f
    }
    def Exp():Variable={
      var output = List[Double]()
      for(x <- data){
        val tmp = exp(x)
        output = output :+ tmp}
      Variable(output)
    }
    override def toString():String="[" + data.toString + "]"
  }
  class Function(val input:Variable*){
    //val x = input.data
    val y = forward()
    val output = Variable(y)
    var ID :String=""
    val name =""
    def forward():List[Double] = throw new NoSuchElementException
    def backward(grad:Variable):Variable = ??? 
    def backward(grad:Variable,x:List[Double]):Variable = ???
    def backward(grad:Variable,x:Variable):Variable = ???
  }

  class Square(override val input:Variable*) extends Function(input:_*) {
    override val name = "^2"
    override def forward():List[Double]={
      val in_data = input(0)
      val output = in_data * in_data
      output.data
    }
    override def backward(grad:Variable):Variable={
      val output = (input(0) +++ input(0)) *** grad
      output
    }
  }
  object Square {
    def apply(name:String,input:Variable):Variable={
      val n = new Square(input)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
 // class Exp(override val input:Variable*) extends Function(input:_*){
 //   override val name = "Exp"
 //   override def forward():List[Double]={
 //     val output = input(0).Exp()
 //     output.data
 //   }
 //   override def backward(grad:Variable(List[Double])):Variable(List[Double])={
 //     val output = (input(0).Exp()) *** grad
 //     output.data
 //   }
 // }
 // object Exp{
 //   def apply(name:String,input:Variable):Variable={
 //     val n = new Exp(input)
 //     n.output.creator = n
 //     n.ID = Id
 //     n.output.ID = Id
 //     n.output.name = name
 //     n.output
 //   }
 // }
  class Add(override val input:Variable*) extends Function(input:_*){
    override val name = "+"
    override def forward():List[Double]={
      var out=List[Double]()
      def small_forward(x1:List[Double],x2:List[Double]):List[Double]=(Variable(x1) + Variable(x2)).data
      for(x <- input) {if(out==List[Double]()) out=x.data; else out = small_forward(out,x.data)}
      out
    }
    override def backward(grad:Variable,x:List[Double])={
      grad
    }
    override def backward(grad:Variable,x:Variable)={
      grad
    }
  }
  object Add{
    def apply(name:String,inputs:Variable*):Variable={
      val n = new Add(inputs:_*)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
  class Sub(override val input:Variable*) extends Function(input:_*){
    override val name = "-"
    override def forward():List[Double]={
      def small_forward(x1:List[Double],x2:List[Double]):List[Double]=(Variable(x1) - Variable(x2)).data
      val out = input(0) - input(1)
      out.data
    }
    override def backward(grad:Variable,x:List[Double])={
      if(input(0).data == x) grad
      else {val out= grad *** (-1);out}
    }
    override def backward(grad:Variable,x:Variable)={
      if(input(0).data == x.data) grad
      else {val out = grad *** (-1); out}
    }
  }
  object Sub{
    def apply(name:String,x0:Variable,x1:Variable):Variable={
      val n = new Sub(x0,x1)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
  class Mul(override val input:Variable*) extends Function(input:_*){
    override val name = "*"
    override def forward():List[Double]={
      var out=List[Double]()
      def small_forward(x1:List[Double],x2:List[Double]):List[Double]=(Variable(x1) *  Variable(x2)).data
      for(x <- input) {if(out==List[Double]()) out=x.data; else out = small_forward(out,x.data)}
      out
    }
    override def backward(grad:Variable,x:List[Double])={
      val out = (grad *** output) \\\ Variable(x,isConstant = true)
      out
    }

    override def backward(grad: Variable, x: Variable) = {
      val out = (grad *** output) \\\ x
      out
    }
  }
  object Mul{
    def apply(name:String,inputs:Variable*):Variable={
      val n = new Mul(inputs:_*)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name=name
      n.output
    }
  }
  class Div(val Divisor:Variable,val Dividend:Variable) extends Function(Divisor,Dividend){
    override val name = "/"
    override def forward():List[Double]={
      var out=List[Double]()
      for((x,y) <- Divisor.data zip Dividend.data) out :+= x / y
      out
    }
    override def backward(grad:Variable,x:List[Double])={
      var out = Variable(List[Double]())
      if(Divisor.data == x) out = (output \\\ Variable(x)) *** grad
      else out = (output \\\ Variable(x)) *** grad *** Variable(List.fill(grad.data.length)(-1.0)) 
      out
    }

    override def backward(grad: Variable, x: Variable) = {
      var out = Variable(List[Double]())
      if (Divisor.data == x.data) out = (output \\\ x) *** grad
      else out = (output \\\ x) *** grad *** Variable(List.fill(grad.data.length)(-1.0),isConstant = true)
      out
    }
  }
  object Div{
    def apply(name:String,Divisor:Variable,Dividend:Variable):Variable={
      val n = new Div(Divisor,Dividend)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
  class Pow(val Modulus:Variable,val power:Variable) extends Function(Modulus,power){
    override val name = "**"
    override def forward():List[Double]={
      var out=List[Double]()
      for((x,y) <- Modulus.data zip power.data) out :+= pow(x, y)
      out
    }
    override def backward(grad:Variable,x:List[Double])={
      var out = Variable(List[Double]())
      if(Modulus.data == x) out = power *** (output \\\ Modulus) *** grad
      else out = {
        var ln_modulus = List[Double]()
        for(a <- Modulus.data){ln_modulus :+= log(a)}
        output *** grad *** Variable(ln_modulus,isConstant = true)}
      out
    }

    override def backward(grad: Variable, x: Variable) = {
      var out = Variable(List[Double]())
      if (Modulus.data == x.data) out = power *** (output \\\ Modulus) *** grad
      else out = {
        var ln_modulus = List[Double]()
        for (a <- Modulus.data) {
          ln_modulus :+= log(a)
        }
        output *** grad *** Variable(ln_modulus,isConstant = true)
      }
      out
    }
  }
  object Pow{
    def apply(name:String,Modulus:Variable,power:Variable):Variable={
      val n = new Pow(Modulus,power)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }

  //function for generating computation graph for single variable
  def get_dot_graph_for_single(y:Variable):String={
    val f = y.creator
    var txt=s""
    txt = txt + s"""${y.ID} [label="${y.name}",color=orange,style=filled]\n"""
    if(f != null){
      val inputs = f.input
      val f_id = f.ID
      val f_name = f.name
      val y_id = y.ID
      txt = txt + s"""$f_id [label="$f_name", color=lightblue,style=filled,shape=box]\n"""
      txt = txt +s"$f_id -> $y_id\n"
      for(input <- inputs){
        val input_id = input.ID
        val input_name = input.name
        txt = txt + s"$input_id -> $f_id\n"
        txt = txt + get_dot_graph_for_single(input)
      }
    }
    txt
  }

  //main function for generating whole computation graph from the output
  def get_dot_graph(y:Variable):String={
    val f = y.creator
    var txt =s""
    txt = txt + s"strict digraph {\n"
    txt = txt + get_dot_graph_for_single(y)
    txt = txt + s"}"

    val fp = new PrintWriter(new File("./sample.dot"))
    fp.write(txt)
    fp.close
    val cmd = "dot -T png sample.dot -o sample.png"
    val output = cmd.!!
    txt
  }

  val x = Variable(List(2.0,1.5,2.0,2.5),"x",Id)
  val y = Variable(List(2.0,1.3,2.9,2.2),"y",Id)
  val z = Variable(List(1.5,1.8,2.2,2.4),"z",Id)
  
  def rosenbrock(input:List[Variable]):Variable ={
    val x0 = input(0)
    val x1 = input(1)
    val out = ((x1 --- (x0 ^^^ 2) ) ^^^ 2) *** 100 +++ ((x0 --- 1) ^^^ 2)
    out
  }
  def anotherF(input:List[Variable]):Variable ={
    // f = x^4 -2*x^2
    val x = input(0)
    val out = x +++ x +++ x
    //val out = (x ^^^ 4) --- ((x *** x) *** 2)
    out
  }
  
  //function for forward and backward, once f --- function was passed as a parameter, and input variables
  //were passed, it will calculate forward and backward.
  def Cal(backward:Boolean,f:List[Variable] =>Variable,input:Variable*):Variable = {
    val out = f(input.toList)
    if(backward) out.backward()
    //for(in <- input) println("grad is "+in.grad.toString)
    out
  }
  //for(i <- (0 to 10)){
  //  println(s"Iteration $i")
  //  Cal(true,anotherF,x)
  //  //Cal(true,rosenbrock,x,y)
  //  println("x0 grad:"+x.grad.toString)
  //  //println("x1 grad:"+y.grad.toString)
  //  

  //  val tmp = x * x * 12 - 4
  //  x.data = (x - ((Variable(x.grad)) / tmp)).data
  //  //x.data = (x - (Variable(x.grad) * 0.001)).data
  //  //y.data = (y - (Variable(y.grad) * 0.001)).data
  //  x.clear_grad()
  //  //y.clear_grad()
  //}
  val o =Cal(true,anotherF,x)
  println("x':"+x.grad.toString)
  val x_grad = x.grad
  //x_grad.backward()
  //println("x'':"+x_grad.grad.toString)

  //println("x1:"+y.data.toString)
}
