package mycode.DeZeroScala

import java.io._
import scala.math._
import scala.sys.process._

object newStepV2 extends App{
  val rand = new scala.util.Random
  def Id =(System.currentTimeMillis+rand.nextInt()).toString
  
  case class Variable(var data:List[Double]=List[Double](),var name:String="",var ID:String=null,val isConstant:Boolean=false,val isInput:Boolean=false){
    var grad:Variable = null
    var creator:Function = null
    def set_grad(g:Double):Unit={
      val old_grad = grad.data
      val new_data = old_grad.map(x=>g)
      grad.data = new_data
      //grad.creator = null
    }

    def backward():Unit={
      val f = creator
      val empty_grad = {
        if(grad == null) true
        else{
          if(grad.data == List[Double]()) true
          else false
        }
      }

      if(empty_grad) grad = Variable(List.fill(data.length)(1.0),name="1(df)",isConstant=true) //this is output layer,adding grad
      if(f != null){
        val A = f.inputA
        val B = f.inputB
        if (B.grad == null && !B.isConstant) B.grad = Variable(List.fill(data.length)(0.0), name = B.name + "'s grad", ID = Id)
        if (A.grad == null && !A.isConstant) A.grad = Variable(List.fill(data.length)(0.0), name = A.name + "'s grad", ID = Id)

        if(! A.isConstant){
          if (A.isInput) {
            if (A.grad.data == List.fill(data.length)(0.0)) A.grad = f.backwardA(grad)
            else A.grad = A.grad +++ f.backwardA(grad)
          }
          else {
            //if (A.grad.creator == null) A.grad = f.backwardA(grad)
            A.grad = f.backwardA(grad)
            A.backward()
          }
        }

        if(! B.isConstant){
          if (B.isInput) {
            if (B.grad.data == List.fill(data.length)(0.0)) B.grad = f.backwardB(grad)
            else B.grad = B.grad +++ f.backwardB(grad)
          }
          else {
            //if (B.grad.creator == null) B.grad = f.backwardB(grad)
            B.grad = f.backwardB(grad)
            B.backward()
          }
        }
      }


    }

    // + - * / ^ Sin Cos are value operation return List[Double]

    def sin(): List[Double] = {
      var output = List[Double]()
      for (x <- data) {
        output = output :+ scala.math.sin(x)
      }
      output
    }

    def cos(): List[Double] = {
      var output = List[Double]()
      for (x <- data) {
        output = output :+ scala.math.cos(x)
      }
      output
    }
    def +(input:Variable):List[Double]={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x+y)}
      output
    }
    def +(input:Double):List[Double]={
      var output = List[Double]()
      for(x <- data){output = output :+ (x+input)}
      output
    }
    def -(input:Variable):List[Double]={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x-y)}
      output
    }
    def -(input:Double):List[Double]={
      var output = List[Double]()
      for(x <- data){output = output :+ (x-input)}
      output
    }
    def *(input:Variable):List[Double]={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x*y)}
      output
    }
    def *(input:Double):List[Double]={
      var output = List[Double]()
      for(x <- data){output = output :+ (x*input)}
      output
    }
    def /(input:Variable):List[Double]={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ (x/y)}
      output
    }
    def /(input:Double):List[Double]={
      var output = List[Double]()
      for(x <- data){output = output :+ (x/input)}
      output
    }
    def ^(input:Variable):List[Double]={
      var output = List[Double]()
      for((x,y) <- data zip input.data){output = output :+ pow(x,y)}
      output
    }
    def ^(input:Double):List[Double]={
      var output = List[Double]()
      for(x <- data){output = output :+ pow(x,input)}
      output
    }
    def ln():List[Double]={
      var output = List[Double]()
      for(x <- data) {output = output :+ log(x)}
      output
    }

    //  +++ --- *** \\\ ^^^ are computation operation which return Variable. It will generate Computation Graph. It is for backward function
    def +++(input:Variable):Variable={
      Add("",this,input)
    }
    def +++(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Add("", this, in)
    }

    def ***(input:Variable):Variable={
      Mul("",this,input)
    }
    def ***(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Mul("", this, in)
    }

    def ---(input:Variable):Variable={
      Sub("",this,input)
    }
    def ---(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Sub("", this, in)
    }

    def \\\(input:Variable):Variable={
      Div("",this,input)
    }
    def \\\(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Div("", this, in)
    }

    def ^^^(input:Variable):Variable={
      Pow("",this,input)
    }
    def ^^^(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Pow("", this, in)
    }

    def lnlnln(input:Variable):Variable={
      Ln(input)
    }
    def lnlnln(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Ln(in)
    }

    def sin(input:Variable): Variable = {
      Sin(input)
    }

    def cos(input: Variable): Variable = {
      Cos(input)
    }
    override def toString():String="[" + data.toString + "]"
  }
  class Function(val inputA:Variable,val inputB:Variable){
    //val x = input.data
    val y = forward()
    val output = Variable(y)
    var ID :String=""
    val name =""
    def forward():List[Double] = throw new NoSuchElementException
    def backwardA(grad:Variable):Variable = ???  //backward function for inputA 
    def backwardB(grad:Variable):Variable= ???   //backward function for inputB
  }

  class Add(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    override val name = "+"
    override def forward():List[Double]={
      inputA + inputB
    }
    override def backwardA(grad:Variable)={
      grad
    }
    override def backwardB(grad:Variable)={
      grad
    }
  }
  object Add{
    def apply(name:String,inputA:Variable,inputB:Variable):Variable={
      val n = new Add(inputA,inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "")  n.output.name =name
      else n.output.name = "( " + inputA.name + " + " + inputB.name + " )"
      n.output
    }
  }
  class Sub(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    override val name = "-"
    override def forward():List[Double]={
      inputA - inputB
    }
    override def backwardA(grad:Variable)={
      grad
    }
    override def backwardB(grad:Variable)={
      grad *** Variable(List.fill(grad.data.length)(-1),name="-1",ID=Id,isConstant=true)

    }
  }
  object Sub{
    def apply(name:String,inputA:Variable,inputB:Variable):Variable={
      val n = new Sub(inputA,inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "") n.output.name = name
      else n.output.name = "( " + inputA.name + " - " + inputB.name + " )"
      n.output
    }
  }
  class Mul(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    override val name = "*"
    override def forward():List[Double]={
      inputA * inputB
    }
    override def backwardA(grad:Variable)={
      inputB *** grad
    }
    override def backwardB(grad:Variable)={
      inputA *** grad
    }
  }
  object Mul{
    def apply(name:String,inputA:Variable,inputB:Variable):Variable={
      val n = new Mul(inputA,inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "")  n.output.name =name
      else n.output.name = "( " + inputA.name + " * " + inputB.name + " )"
      n.output
    }
  }
  class Div(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    override val name = "/"
    override def forward():List[Double]={
      inputA / inputB
    }
    override def backwardA(grad:Variable)={
      grad \\\ inputB
    }
    override def backwardB(grad:Variable)={
      inputA *** Variable(List.fill(grad.data.length)(-1),name="-1",isConstant=true) *** ( inputB ^^^ -2) *** grad
    }
  }
  object Div{
    def apply(name:String,Divisor:Variable,Dividend:Variable):Variable={
      val n = new Div(Divisor,Dividend)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "")  n.output.name =name
      else n.output.name = "( " + Divisor.name + " / " + Dividend.name + " )"
      n.output
    }
  }
  class Pow(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    override val name = "^"
    override def forward():List[Double]={
      inputA ^ inputB
    }
    override def backwardA(grad:Variable)={
      val one = Variable(List.fill(grad.data.length)(1),name="1",ID=Id,isConstant = true)
      val B = inputB --- one
      inputB *** Pow("",inputA,B) *** grad
    }
    override def backwardB(grad:Variable)={
      //val ln_A = inputA lnlnln inputA
      (inputA ^^^ inputB) *** Ln(inputA) *** grad
    }
  }
  object Pow{
    def apply(name:String,inputA:Variable,inputB:Variable):Variable={
      val n = new Pow(inputA,inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "")  n.output.name =name
      else n.output.name = "( " + inputA.name + " ^ " + inputB.name + " )"
      n.output
    }
  }
  class Ln(override val inputA:Variable,override val inputB:Variable) extends Function(inputA,inputB){
    // it implement ln(inputB). Nothing to do with inputA
    override val name = "ln"
    override def forward():List[Double]={
      inputB.ln()
    }
    override def backwardA(grad:Variable)={
      Variable(List.fill(grad.data.length)(0),isConstant=true)
    }
    override def backwardB(grad:Variable)={
      Variable(List.fill(grad.data.length)(1),name="1",isConstant=true) \\\ inputB *** grad
    }
  }
  object Ln{
    def apply(inputB:Variable):Variable={
      val inputA = Variable(inputB.data,isConstant=true)
      val name = ""
      val n = new Ln(inputA,inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if(name!= "")  n.output.name =name
      else n.output.name = "( ln" + inputB.name + " )"
      n.output
    }
  }

  class sin(override val inputA: Variable, override val inputB: Variable) extends Function(inputA, inputB) {
    // it implement ln(inputB). Nothing to do with inputA
    override val name = "sin"

    override def forward(): List[Double] = {
      inputB.sin()
    }

    override def backwardA(grad: Variable) = {
      Variable(List.fill(grad.data.length)(0),name="0",isConstant = true)
    }

    override def backwardB(grad: Variable) = {

      //grad *** Cos(inputB)
      val n = new cos(inputB, inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      n.output.name = "( cos " + inputB.name + " )"
      n.output *** grad
    }
  }

  object Sin {
    def apply(inputB: Variable): Variable = {
      val name = ""
      val inputA = Variable(inputB.data, isConstant = true)
      val n = new sin(inputA, inputB)
      //val n =Variable(inputB.sin())
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if (name != "") n.output.name = name
      else n.output.name = "( Sin " + inputB.name + " )"
      n.output
    }
  }

  class cos(override val inputA: Variable, override val inputB: Variable) extends Function(inputA, inputB) {
    // it implement ln(inputB). Nothing to do with inputA
    override val name = "sin"

    override def forward(): List[Double] = {
      inputB.cos()
    }

    override def backwardA(grad: Variable) = {
      Variable(List.fill(grad.data.length)(0),name="0",isConstant = true)
    }

    override def backwardB(grad: Variable) = {

      val n = new sin(inputB, inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      n.output.name = "( sin " + inputB.name + " )"
      n.output *** grad *** Variable(List.fill(grad.data.length)(-1), name = "-1", isConstant = true)
    }
  }

  object Cos {
    def apply(inputB: Variable): Variable = {
      val name = ""
      val inputA = Variable(inputB.data, isConstant = true)
      val n = new cos(inputA, inputB)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      val t = n.output.ID
      if (name != "") n.output.name = name
      else n.output.name = "( cos " + inputB.name + " )"
      n.output
    }
  }

  def get_dot_graph_for_single(y:Variable):String={
    val f = y.creator
    var txt=s""
    txt = txt + s"""${y.ID} [label="${y.name}",color=orange,style=filled]\n"""
    if(f != null){
      val inputs = List(f.inputA,f.inputB)
      val f_id = f.ID
      val f_name = f.name
      val y_id = y.ID
      txt = txt + s"""$f_id [label="$f_name", color=lightblue,style=filled,shape=box]\n"""
      txt = txt +s"$f_id -> $y_id\n"
      for(input <- inputs){
        val input_id = input.ID
        val input_name = input.name
        if(input.isConstant) txt = txt + s"$input_id -> $f_id[color=red]\n"
        else txt = txt + s"$input_id -> $f_id[color=black]\n"
        txt = txt + get_dot_graph_for_single(input)
      }
    }
    txt
  }

  def get_dot_graph(y:Variable,fn:String="sample"):String={
    if(y.creator == null){

    }
    val f = y.creator
    var txt =s""
    txt = txt + s"strict digraph {\n"
    txt = txt + get_dot_graph_for_single(y)
    txt = txt + s"}"

    val filename="./" +fn + ".dot"
    val fp = new PrintWriter(new File(filename))
    fp.write(txt)
    fp.close
    val cmd = "dot -T png " + fn + ".dot" + " -o " + fn + ".png"
    val output = cmd.!!
    txt
  }

  var x = Variable(List(Pi/6,1,1,1),"x",Id,isInput = true)
  val y = Variable(List(Pi/6,1.3,2.9,2.2),"y",Id,isInput = true)
  val z = Variable(List(1.5,1.8,2.2,2.4),"z",Id,isInput = true)

  //function for forward and backward, once f --- function was passed as a parameter, and input variables
  //were passed, it will calculate forward and backward.
  def Cal(backward: Boolean, f: List[Variable] => Variable, input: Variable*): Variable = {
    val out = f(input.toList)
    if (backward) out.backward()
    //for(in <- input) println("grad is "+in.grad.toString)
    out
  }
  //failed function
  val f =Ln(Sin(x) *** Sin(y))


  //passed function
  //val f = (x ^^^ 2) +++ (x ^^^ -3)
  //val f = Ln(x +++ 3)
  //val f = Ln(x ^^^ 2)
  //val f = Ln((x *** x) +++ (x *** 2))
  //val f = Ln(Sin(x))
  get_dot_graph(f,"f.jpg")
  f.backward()
  println(f.name + ":")
  println("x' is "+x.grad.name)
  println("x' is " + x.grad.data.toString())
  println("y' is "+ y.grad.data.toString())
  //get_dot_graph(y.grad,"gy1")
  x.set_grad(0)
  y.set_grad(0)
  //对于多参数的微分，在进行二次微分时，要保存其它参数的creator，用于将来它们自己的二次微分。
  val y_grad_creator = y.grad.creator
  get_dot_graph(x.grad,"gx")
  x.grad.backward()
  println("x'' is " + x.grad.name)
  println("x'' is "+x.grad.data.toString())
  get_dot_graph(x.grad,"gx2")
  x.set_grad(0)
  //参数的还原
  y.grad.creator = y_grad_creator
  y.set_grad(0)
  y.grad.backward()
  println("y'' is " + y.grad.data.toString())
  //get_dot_graph(y.grad,"gy2")
}
