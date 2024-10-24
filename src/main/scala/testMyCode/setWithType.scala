package testMyCode
trait Ordered[A]{
  def compare(that: A): Int
  def < (that: A): Boolean =(this compare that) < 0
  def > (that: A): Boolean =(this compare that) > 0
  def <= (that: A): Boolean =(this compare that) <= 0
  def >= (that: A): Boolean =(this compare that) >= 0
  def compareTo(that: A): Int = compare(that)
}


abstract class Set[A <: Ordered[A]]{
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
  override def toString(): String
}

class EmptySet[A <: Ordered[A]] extends Set[A]{
  def contains(x: A) = false
  def incl(x: A) = new NonEmptySet[A](x,new EmptySet[A],new EmptySet[A])
  override def toString() = ""
}

class NonEmptySet[A <: Ordered[A]](elem: A,left: Set[A],right: Set[A]) extends Set[A]{
  
  def contains(x: A)=
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true
  def incl(x: A): Set[A]=
    if(x < elem) new NonEmptySet[A](elem,left incl x, right)
    else if(x > elem) new NonEmptySet[A](elem, left, right incl x)
    else this
  override def toString(): String = left.toString + " " + elem.toString + " " + right.toString

}
case class Num(value: Double) extends Ordered[Num]{
  def compare(that: Num): Int =
    if(this.value < that.value) -1
    else if(this.value > that.value) 1
    else 0
}

object SetWithTypeApp extends App{
  println(new EmptySet[Num].incl(Num(1)).incl(Num(3)).incl(Num(5)))
}
