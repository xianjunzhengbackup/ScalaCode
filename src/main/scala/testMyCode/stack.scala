package testMyCode

abstract class Stack[+A]{
  def push[B >:A](x: B): Stack[B]= new NonEmptyStack[B](x,this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
  override def toString(): String 
}
object EmptyStack extends Stack[Nothing]{
  def isEmpty = true
  def top = ???
  def pop = ???
  override def toString() = ""
}

class NonEmptyStack[+A](elem: A,rest: Stack[A]) extends Stack[A]{
  def isEmpty = false
  def top = elem
  def pop = rest
  override def toString() = elem.toString + " " + rest.toString()
}
object stackApp extends App{
  println(EmptyStack.push("abc").push(new AnyRef()))
}
