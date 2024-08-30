package mycode.DeZeroScala
import scala.math.exp
import java.io._
import scala.sys.process._

object step19 extends App{
  val rand = new scala.util.Random
  def Id =(System.currentTimeMillis+rand.nextInt()).toString
  case class Variable(val data:List[Double],var name:String="",var ID:String=null){
    var grad = List[Double]()
    var creator:Function = null

    def backward():Unit={
      val f = creator
      val empty_grad = grad == List[Double]()
      val reach_first_layer = f == null
      if(empty_grad) grad = List.fill(data.length)(1.0) //this is output layer,adding grad
      if(!reach_first_layer){
        val xs = f.input
        val single_input = xs.size == 1
        if(single_input) {
          //Inintialization for grad
          if(xs(0).grad == List[Double]()) xs(0).grad = List.fill(data.length)(0.0)
          //println("input is:"+xs)
          //println("old grad is:"+xs(0).grad.toString)
        
          //Reach the first layer,grad need accumulated.Otherwise just copy
          if(xs(0).creator == null) xs(0).grad = (Variable(f.backward(grad)) + Variable(xs(0).grad)).data
          else xs(0).grad = f.backward(grad)
          //println("new grad is:"+xs(0).grad.toString)
          xs(0).backward()
        } else{
          for(x <- xs){
            //Inintialization for grad
            if(x.grad == List[Double]()) x.grad = List.fill(data.length)(0.0)
            //println("input is:"+x)
            //println("old grad is:"+x.grad.toString)
            
            //Reach the first layer, grad need accumulated. Otherwise just copy
            if(x.creator == null) x.grad = (Variable(f.backward(grad,x.data)) + Variable(x.grad)).data
            else x.grad = f.backward(grad,x.data)
            //println("new grad is:"+x.grad.toString)
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
    var ID :String=""
    val name =""
    def forward():List[Double] = throw new NoSuchElementException
    def backward(grad:List[Double]):List[Double] = ??? 
    def backward(grad:List[Double],x:List[Double]):List[Double] = ???
  }

  class Square(override val input:Variable*) extends Function(input:_*) {
    override val name = "^2"
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
    def apply(name:String,input:Variable):Variable={
      val n = new Square(input)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
  class Exp(override val input:Variable*) extends Function(input:_*){
    override val name = "**"
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
    def apply(name:String,input:Variable):Variable={
      val n = new Exp(input)
      n.output.creator = n
      n.ID = Id
      n.output.ID = Id
      n.output.name = name
      n.output
    }
  }
  class Add(override val input:Variable*) extends Function(input:_*){
    override val name = "+"
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
    def apply(name:String,inputs:Variable*):Variable={
      val n = new Add(inputs:_*)
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
    override def backward(grad:List[Double],x:List[Double])={
      val out = (Variable(grad) * output) / Variable(x)
      out.data
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
    override def backward(grad:List[Double],x:List[Double])={
      var out = Variable(List[Double]())
      if(Divisor.data == x) out = output / Variable(x) * Variable(grad)
      else out = output / Variable(x) * (Variable(grad) * Variable(List.fill(grad.length)(-1.0))) 
      out.data
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

  def Cal(backward:Boolean,inputs:Variable*):Variable={
    val x = inputs(0)
    val y = inputs(1)
    val z = inputs(2)

    val a = Square("a",x)
    
    val b = Square("b",y)
    val c = Square("c",z)

    val f = Exp("f",a)
    val d = Mul("d",a,b)
    val e = Div("e",a,c)

    val h = Mul("h",f,d)
    val g = Add("g",d,e,a)

    val j = Div("j",g,d)

    val out = Add("out",h,j,a)

    if(backward){
      println(out.data)
      out.backward()
      println("Backward....")
      println("x's grad:"+x.grad.toString)
      println("y's grad:"+y.grad.toString)
      println("z's grad:"+z.grad.toString)
    }
    val dot = get_dot_graph(out)
    
    out
  } 
  def anotherDiff(inputs:Variable*):Unit={
    val x = inputs(0)
    val y = inputs(1)
    val z = inputs(2)
    val eps = Variable(List.fill(x.data.length)(1e-4))
    val x1 =x - eps
    val x2 = x + eps
    val y1 = y - eps
    val y2 = y + eps
    val z1 = z - eps
    val z2 = z + eps

    val o_x1 = Cal(false,x1,y,z)
    val o_x2 = Cal(false,x2,y,z)
    println("1st grad:"+((o_x2 - o_x1) / (eps*2)).data)

    val o_y1 = Cal(false,x,y1,z)
    val o_y2 = Cal(false,x,y2,z)
    println("2nd grad:"+((o_y2 - o_y1) / (eps*2)).data)

    val o_z1 = Cal(false,x,y,z1)
    val o_z2 = Cal(false,x,y,z2)
    println("3rd grad:"+((o_z2 - o_z1) / (eps*2)).data)
  }
  val x = Variable(List(0.5,1.0,2.0,2.5),"x",Id)
  val y = Variable(List(2.5,1.3,2.9,2.2),"y",Id)
  val z = Variable(List(1.5,1.8,2.2,2.4),"z",Id)
  
  Cal(true,x,y,z)
  anotherDiff(x,y,z)
}
