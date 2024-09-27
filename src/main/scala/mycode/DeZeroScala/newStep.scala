package mycode.DeZeroScala
import scala.math._
import java.io._
import scala.sys.process._

object newStep extends App{
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
      val reach_first_layer = isInput
      if(empty_grad) grad = Variable(List.fill(data.length)(1.0),name="1(df)",isConstant=true) //this is output layer,adding grad
      if(!reach_first_layer){
        val A = f.inputA
        val B = f.inputB
        val single_input = A.isConstant | B.isConstant //as long as one of two inputs has one constant,it is single input
        if(single_input) {
          if(A.isConstant){
            //grad value initialization
            if(B.grad == null) B.grad = Variable(List.fill(data.length)(0.0),name=B.name+"'s grad",ID=Id)
            //reach the input layer. So the grad for input layer need to be accumulated
            if(B.grad.creator==null){
              if (B.creator == null) B.grad = f.backwardB(grad) +++ B.grad
              //Other layer except input layer, just copy the value then pass the grad
              else B.grad = f.backwardB(grad)
            }
            B.backward()
          } else{
            if(A.grad == null) A.grad = Variable(List.fill(data.length)(0.0),name=A.name+"'s grad",ID=Id)
            if(A.grad.creator==null){
              if (A.creator == null) A.grad = f.backwardA(grad) +++ A.grad
              else A.grad = f.backwardA(grad)
            }
            A.backward()
          }
        } else{
          if(A.grad == null) A.grad = Variable(List.fill(data.length)(0.0),name=A.name+"'s grad",ID=Id)
          if(B.grad == null) B.grad = Variable(List.fill(data.length)(0.0),name=B.name+"'s grad",ID=Id)

          //if(A.grad.creator == null){
            //if (A.creator == null) A.grad = f.backwardA(grad) +++ A.grad
            //else A.grad = f.backwardA(grad)
          //}
          A.grad = f.backwardA(grad) +++ A.grad
          //if(B.grad.creator==null){
          //  if (B.creator == null) B.grad = f.backwardB(grad) +++ B.grad
          //  else B.grad = f.backwardB(grad)
          //}
          //A.backward()
          //B.backward()
          B.grad = f.backwardB(grad) +++ B.grad
        }
      } else {
        
      }
    }

    // + - * / ^ are value operation return List[Double]
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
      Ln("",input)
    }
    def lnlnln(input: Double): Variable = {
      val in:Variable = Variable(List.fill(data.length)(input),ID=Id,name=input.toString,isConstant=true)
      Ln("", in)
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
      inputA *** Variable(List.fill(grad.data.length)(-1),isConstant=true) \\\ inputB \\\ inputB *** grad
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
      val ln_A = inputA lnlnln inputA
      (inputA ^^^ inputB) *** ln_A *** grad
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
      Variable(List.fill(grad.data.length)(1),isConstant=true) \\\ inputB *** grad
    }
    override def backwardB(grad:Variable)={
      Variable(List.fill(grad.data.length)(1),isConstant=true) \\\ inputB *** grad
    }
  }
  object Ln{
    def apply(name:String,inputB:Variable):Variable={
      val inputA = Variable(inputB.data,isConstant=true)
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
      txt = txt + s"$input_id -> $f_id\n"
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

  var xx = Variable(List(2,1,1,1),"x",Id,isInput = true)
  val yy = Variable(List(2.5,1.3,2.9,2.2),"y",Id,isInput = true)
  val z = Variable(List(1.5,1.8,2.2,2.4),"z",Id,isInput = true)

  //function for forward and backward, once f --- function was passed as a parameter, and input variables
  //were passed, it will calculate forward and backward.
  def Cal(backward: Boolean, f: List[Variable] => Variable, input: Variable*): Variable = {
    val out = f(input.toList)
    if (backward) out.backward()
    //for(in <- input) println("grad is "+in.grad.toString)
    out
  }
  def myFun(input:List[Variable] ):Variable= {
    val x = input(0)
    val out = x ^^^ 4
    out
  }
  for(iteration <- (0 to 0)){
    //this is how it works
    // everytime need to create a new x with copy method.Otherwise 2nd iteration gx2 won't be right.
    println(s"Iteration :$iteration:")
    val x = xx.copy()
    val y = yy.copy()
    println("x is " + x.data.toString)
    //val out = Cal(true,myFun,xx)
    val out = (x ^^^ 3)
    out.backward()
    get_dot_graph(x.grad,"x_grad1")
    get_dot_graph(out,"out1")
    //get_dot_graph(y.grad,"y_grad1")
    //val gx = x.grad.copy()
    println("x' is " + x.grad.data.toString)
    //println("y' is " + y.grad.data.toString())
    //y.grad.data = List[Double]()
    x.set_grad(0)
    println("x grad name " + x.grad.name)
    x.grad.backward()
    println("x'' is "+x.grad.data.toString())


    //x.grad.backward()
    //println("x'' is(2nd time) " + x.grad.data.toString())


    //get_dot_graph(x.grad.grad,"x_grad2")

    //x.grad.data = List[Double]()
    //y.grad.data = List[Double]()
    //y.grad.backward()
    //println("y'' is "+y.grad.data.toString())
    //get_dot_graph(y.grad,"y_grad2")
    //y.grad.backward()
    //println("y'' is" + y.grad.data.toString())


    //get_dot_graph(x.grad.grad,"gx2_1")
    //val gx2 = x.grad
    //val newXData = x - Variable(gx / gx2)
    //xx.data = newXData
    //println("gx2 is" + x.grad.data.toString())
    //println("new x:" + newXData.toString)
    //x.set_grad(0.0)
    //x.grad.backward()
    //get_dot_graph(x.grad.grad,"gx2_2")
    //println("gx2 is "+ x.grad.data.toString())
  }



}
