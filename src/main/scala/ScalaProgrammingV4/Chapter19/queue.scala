package ScalaProgrammingV4.Chapter19

trait Queue[T]{
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue{
  def apply[T](xs: T*): Queue[T]=
    new QueueImpl[T](xs.toList,Nil)

  private class QueueImpl[T](
    private val leading: List[T],
    private val trailing: List[T]) extends Queue[T]{
      def mirror=
        if(leading.isEmpty)
          new QueueImpl(trailing.reverse,Nil)
        else
          this

      def head: T=mirror.leading.head
      def tail: QueueImpl[T]={
        val q= mirror
        new QueueImpl(q.leading.tail,q.trailing)
      }
      def enqueue(x: T)=
        new QueueImpl(leading,x :: trailing)
      override def toString()= leading.foldLeft("")((s,x)=>s+" "+x.toString) + trailing.reverse.foldLeft("")((t,y)=>t+" "+y.toString)
    }

}

object QueueApp extends App{
  val a = Queue(1,2,3,4,5)
  println(a.head)
  println(a.tail)
  println(a enqueue 6)
}
