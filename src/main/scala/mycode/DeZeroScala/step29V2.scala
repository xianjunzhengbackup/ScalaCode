package mycode.DeZeroScala
import scala.math._
import java.io._
import scala.sys.process._

object step29V2 extends App{
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
              //*********************************************************************
              //problem is here. x.grad as a variable, its creator is null, it should point to f.backward
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
  val x = Variable(List(2.0,1.5,2.0,2.5),"x",ID = Id)
  val out = x +++ x +++ x
  out.backward()
  println("x':"+x.grad.toString)
}
