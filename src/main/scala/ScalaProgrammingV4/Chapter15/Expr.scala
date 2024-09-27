package ScalaProgrammingV4.Chapter15
import math.{E,Pi}

sealed abstract class Expr
case class Var(name:String) extends Expr
case class Number(num:Double) extends Expr
case class UnOp(operator:String,arg:Expr) extends Expr
case class BinOp(operator:String,left:Expr,right:Expr) extends Expr {
  override def toString = left.toString + " " + operator + " " +right.toString
}

object ExprObj extends App{
  val aVar = Var("a")
  val aNum = Number(19)
  val aRes = BinOp("+",aVar,aNum)
  println(aRes)

  E match{
    case Pi =>println("strange math? Pi=" + Pi)
    case _ =>println("OK")
  }
  /*
   * Pi以大写字母开头，所以这是常量匹配模式
   * 结果将打印 OK
   */

  val pi = math.Pi
  E match{
    case pi =>println("strange math? Pi=" + pi)
    case _ =>println("OK")
  }
  /*
   * pi以小写字母开头，所以这是变量匹配模式。E将被赋值给变量pi。
   * 结果将打印 strange math? Pi=2.718281828459045
   */

  E match{
    case `pi` =>println("strange math? Pi=" + pi)
    case _ =>println("OK")
  }
  /*
   * 用了`pi`，将强制scala将pi看成是常量，也就是强制进入常量模式。
   * 结果是 OK
   */

  aRes match{
    case BinOp("+",e,Number(num)) => println("a deep match")
    case _ =>
  }
  /*
   *这个就是构造方法匹配
   */

  List(0,1,2,3,4) match{
    case List(0,_,_,_,_) => println("found it")
    case _ =>
  }
  /*
   *序列模式匹配，将匹配一个5元数组，以 0 开头
   */
  List(0,1,2) match{
    case List(0,_*) =>println("found it")
    case _=>
  }
  /*
   *将匹配任意长度，且以0 开头的数组
   */

  List("string",Map(1->'a',2->'b'),4).foreach{
    case s:String =>println(s.length)
    case m:Map[_,_] =>println(m.size)
    case _ => println(-1)
  }
  /*
   * 这个叫类型匹配
   */
  UnOp("abs", UnOp("abs",Number(-1))) match{ 
    case UnOp("abs",e @ UnOp("abs",_)) => println(e)
    case _=>
  }
  /*
   * 变量绑定。嵌套的匹配，外层匹配后，如果里面也匹配 UnOp("abs",_），那么里面的这层将赋值给 e
   */

  /*底下这段代码就是想实现e + e 让它变成 2*e，但是编译器报错了。编译器认为在 println(BinOp("x",x,Number(2)))中x 又出现了一次。
   * 要解决这个问题，就要用模式守卫。
   * BinOp("+",Number(1),Number(1)) match{
    case BinOp("+",x,x) => println(BinOp("*",x,Number(2)))
    case _ => 
  }
  case BinOp("+",x,x) => println(BinOp("*",x,Number(2)))
[error]    |                     ^
[error]    |                     duplicate pattern variable: x

  */
  BinOp("+",Number(1),Number(1)) match{
    case BinOp("+",x, y) if x==y => println(BinOp("*",x,Number(2)))
    case _ => 
  }
  /*
   *这里 的 if x＝＝y 就是模式守卫
   */

  /*
   *密封类
   在用 match case 的时候，一个潜在的问题是，你需要覆盖所有可能的场景，所以通常都会在末尾添加一个 _=> 来处理这些情况。假如没有这个 _=>，我们希望编译器能告诉我们错过了一些 case。这就是 sealed 类存在的理由。
    密封类的所有子类都必须定义在同一个文件中。这就是为什么 sealed 类通常用来进行模式匹配。
    但有些地方，我知道有些模式匹配它肯定不会发生，比如下面 describe 函数，它只匹配Number和 Var，其它的匹配在这里不会发生。所以这里用了@unchecked 注解，那么编译器将不会对它的模式匹配去查是否覆盖了全范围。
   */
  def describe(e:Expr):String =(e: @unchecked) match{
    case Number(_) => "a Number"
    case Var(_) => "a Variable"
  }
}
