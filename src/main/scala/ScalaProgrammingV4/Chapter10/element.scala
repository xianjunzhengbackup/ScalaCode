package ScalaProgrammingV4.Chapter10

abstract class Element{
  def contents: Array[String]
  def height: Int=contents.length
  def width:Int = if(height == 0) 0 else contents(0).length
  def above(that:Element):Element =
    Element(this.contents ++ that.contents)
  def beside(that:Element):Element =
    Element(
      for(
           (line1,line2) <- this.contents zip that.contents
         ) yield line1 + line2
  )
  override def toString = contents mkString "\n"
}

object Element {
  def apply(contents:Array[String]):ArrayElement=
    new ArrayElement(contents)

  def apply(s:String):LineElement =
    new LineElement(s)

  def apply(ch:Char,
    width:Int,
    height:Int):UniformElement =
      new UniformElement(ch,width,height)
}

class ArrayElement(val contents:Array[String]) extends Element{

}

class LineElement(s:String) extends Element{
  def contents = Array(s)
  override def width = s.length
  override def height = 1
}

class UniformElement(ch:Char,
  override val width:Int,
  override val height:Int) extends Element{
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }

object chapter10 extends App{
  println("Chapter 10")
  val ae = new ArrayElement(Array("hello","world"))
  println(ae.height)
  println(ae.width)
  
  println(Element(Array("hello","world")) above Element(Array("1","2","3")))
  println(Element(Array("hello","world")) beside Element(Array("1","2","3")))

  println(Element('c',4,8))

  println(Element("lllll"))
}
