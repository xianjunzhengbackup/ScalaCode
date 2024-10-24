package testMyCode

/*
 题目描述：
10个房间里放着随机数量的金币。每个房间只能进入一次，并只能在一个房间中拿金币。一个人采取如下策略：前4个房间只看不拿。随后的房间只要看到比前4个房间都多的金币数就拿。否则就拿最后一个房间的金币。编程计算这种策略拿到最多金币的概率。
分析与解答：
这道题是一个求概率的问题。由于10个房间里放的金币的数量是随机的，因此在编程实现时首先需要生成10个随机数来模拟10个房间里金币的数量。然后判断通过这种策略是否能拿到最多的金币。如果仅仅通过一次模拟来求拿到最多金币的概率显然是不准确的，那么就需要进行多次模拟，通过记录模拟的次数m，拿到最多金币的次数n，从而可以计算出拿到最多金币的概率n/m。显然这个概率与金币的数量以及模拟的次数有关系。模拟的次数越多越能接近真实值。
 */
import scala.util.Random.nextInt

class Rooms {
  val temp = for(i<-0 to 9) yield nextInt(10)
  val golds: List[Int]=temp.toList
  val maxGoldinRooms = golds.max
  val maxGoldinSliding4Rooms = List(11,11,11,11) ++ golds.sliding(4).map(l=>l.max).toList
  val combinedList = golds zip maxGoldinSliding4Rooms 
  val PotentialGolds=combinedList.dropWhile(l=>l._1 <= l._2)
  val fetchGold = PotentialGolds.size match{
    case 0=> golds(9)
    case _=> PotentialGolds(0)._1
  }
}

object goldCoinApp extends App{
  var counter: Int = 0
  for(i<-0 to 100000) {
    val a = new Rooms
    if(a.fetchGold == a.maxGoldinRooms) counter = counter + 1
    println(s"$i: fetch gold is ${a.fetchGold} and max gold in room is ${a.maxGoldinRooms}")
  }
  println("------------------------------------------------")
  val p=counter / 100000.0
  println(s"The possibility using this strategy is $p and counter is $counter")
}
