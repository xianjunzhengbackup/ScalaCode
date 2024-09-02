package ScalaProgrammingV4.Chapter6

class Rational(n:Int,d:Int){
  require(d!=0)
  println("Created "+n+"/"+d)

  val numer = n
  val denom = d

  override def toString=s"$n/$d"

  def add(that:Rational):Rational=
    new Rational(
      numer * that.denom + that.numer*denom,
      denom * that.denom
    )

  def lessThan(that:Rational)=
    numer * that.denom < that.numer * denom

  def max(that:Rational):Rational=
    if(this lessThan that) then that
    else this

  def this(n:Int)=this(n,1)
}

object RationalRun extends App{

  val half=new Rational(1,2)
  val quarter = new Rational(1,4)
  println(half add quarter)
  println(half max quarter)
  val five=new Rational(5)
}


