package mycode.DeZeroScala

object step02 extends App{

  //val l = List(11.0,2.1,3.4444,4.56789012)
  //println(l.toString)
  case class Variable(val data:List[Double]){

  }
  
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
      /*for(data <- in_data){
        val temp = data * data
        container = container :+ temp

      }*/
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
  val data = List(3.0,1.45,4.78,8.90)
  val x = Variable(data)
  println(x.data)

  val f = new Square(x)
  println(f.output.data)

  val f2 = Square(x)
  println(f2.data)
}
