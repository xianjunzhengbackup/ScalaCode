package ScalaProgrammingV4.Chapter6

implicit def IntToRational(x:Int):Rational=new Rational(x)

class Rational(n:Int,d:Int){
  require(d!=0)


  private val g = gcd(n,d).abs
  val numer = n/g
  val denom = d/g
  //println("Created "+numer+"/"+denom)

  override def toString=s"$numer/$denom"

  def +(that:Rational):Rational=
    new Rational(
      numer * that.denom + that.numer*denom,
      denom * that.denom
    )
  def +(that:Int):Rational=this + (new Rational(that))
  def -(that:Rational):Rational=
    new Rational(
      numer * that.denom - that.numer*denom,
      denom * that.denom
    )
  def -(that:Int):Rational=this - (new Rational(that))

  def *(that:Rational):Rational=
    new Rational(
      numer * that.numer,
      denom * that.denom
    )
  def *(that:Int):Rational= this * (new Rational(that))
  def /(that:Rational):Rational=
    new Rational(
      numer * that.denom,
      denom * that.numer
    )
  def /(that:Int):Rational= this / (new Rational(that))

  def lessThan(that:Rational)=
    numer * that.denom < that.numer * denom

  def max(that:Rational):Rational=
    if(this lessThan that) then that
    else this

  def this(n:Int)=this(n,1)

  def gcd(a3:Int,a4:Int):Int =
    if(a4==0) a3 else gcd(a4,a3%a4)
}

object RationalRun extends App{

  val half=new Rational(1,2)
  val quarter = new Rational(1,4)
  println(half + quarter)
  println(half max quarter)
  val five=new Rational(5)

  val a = half + quarter * half
  val b = half * quarter

  val c = a / 5
  val d = b * 5
  println(a)
  println(c)
  println(5 / a)
}


