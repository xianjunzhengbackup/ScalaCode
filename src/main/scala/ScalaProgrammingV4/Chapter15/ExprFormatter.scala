package ScalaProgrammingV4.Chapter15
import ScalaProgrammingV4.Chapter10.Element 

sealed abstract class Expr
case class Var(name:String) extends Expr
case class Number(num:Double) extends Expr
case class UnOp(operator:String,arg:Expr) extends Expr
case class BinOp(operator:String,left:Expr,right:Expr) extends Expr {
  override def toString = left.toString + " " + operator + " " +right.toString
}

class ExprFormatter{
  private val opGroups = 
    Array(
        Set("|","||"),
        Set("&","&&"),
        Set("^"),
        Set("==","!="),
        Set("<","<=",">",">="),
        Set("+","-"),
        Set("*","%")
      )

  private val precedence={
    val assocs =
      for{
        i <-0 until opGroups.length
        op <- opGroups(i)
      }yield op->i
    assocs.toMap
  }
  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr,enclPrec: Int): Element =
    e match{
      case Var(name) => Element(name)
      case Number(num) =>
        def stripDot(s:String) =
          if(s endsWith ".0") s.substring(0,s.length-2)
          else s
        Element(stripDot(num.toString))
      case UnOp(op,arg) => Element(op) beside format(arg,unaryPrecedence)
      case BinOp("/",left,right)=>
        val top=format(left,fractionPrecedence)
        val bot=format(right,fractionPrecedence)
        val line=Element('-',top.width max bot.width,1)
        val frac= top above line above bot
        if(enclPrec != fractionPrecedence) frac
        else Element(" ") beside frac beside Element(" ")
      case BinOp(op,left,right)=>
        val opPrec=precedence(op)
        val l=format(left,opPrec)
        val r=format(right,opPrec+1)
        val oper=l beside Element(" "+op+" ") beside r
        if(enclPrec <= opPrec) oper
        else Element("(") beside oper beside Element(")")
    
    }
  def format(e:Expr):Element =format(e,0)

}

object ExprFormatterObj extends App{
  val demo = new ExprFormatter

  val expr1 = BinOp("+",Number(3.0),Number(4.0))
  val a =Number(3.000)
  val b=Var("xxx")
  val c=UnOp("-",Var("xx"))
  val d = BinOp("/", c, a)
  val e = BinOp("+",d,a)
  println(demo format d)
  println(demo format e)
}
