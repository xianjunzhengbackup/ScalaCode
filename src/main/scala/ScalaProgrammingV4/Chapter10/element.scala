package ScalaProgrammingV4.Chapter10

abstract class Element{
  def contents: Array[String]
  def height: Int=contents.length
  def width:Int = if(height == 0) 0 else contents(0).length
  def above(that:Element):Element ={
    val this1 = this widen that.width
    val that1 = that widen this.width
    Element(this1.contents ++ that1.contents)
  }
  def beside(that:Element):Element ={
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    Element(
      for(
           (line1,line2) <- this1.contents zip that1.contents
         ) yield line1 + line2
  )}
  override def toString = contents mkString "\n"
  def widen(w:Int):Element=
    if(w <= width) this
    else{
      val left = Element(' ',(w-width)/2,height)
      val right = Element(' ',w - width - left.width,height)
      left beside this beside right
    }

  def heighten(h:Int):Element=
    if(h <= height) this
    else {
      val top = Element(' ',width,(h - height)/2)
      val bot = Element(' ',width, h - height - top.height)
      top above this above bot
    }
}

object Element {
  private class ArrayElement(val contents:Array[String]) extends Element

  private class LineElement(s:String) extends Element{
  def contents = Array(s)
    override def width = s.length
    override def height = 1
  }

  private class UniformElement(ch:Char,
    override val width:Int,
    override val height:Int) extends Element{
      private val line = ch.toString * width
      def contents = Array.fill(height)(line)
    }
  def apply(contents:Array[String]):Element=
    new ArrayElement(contents)

  def apply(s:String):Element =
    new LineElement(s)

  def apply(ch:Char,
    width:Int,
    height:Int):Element =
      new UniformElement(ch,width,height)
}

object chapter10 extends App{
  println("Chapter 10")
  val ae = Element(Array("hello","world"))
  println(ae.height)
  println(ae.width)
  
  println(Element(Array("hello","world")) above Element(Array("1","2","3")))
  println(Element(Array("hello","world")) beside Element(Array("1","2","3")))

  println(Element('c',4,8) above Element(Array("1","2")))
  println(Element('c',4,8) beside Element('d',2,3))

}
