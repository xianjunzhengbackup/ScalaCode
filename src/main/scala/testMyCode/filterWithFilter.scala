package testMyCode
case class Programmer(name: String,
                      level: Level,
                      knownLanguages: List[String])
sealed trait Level
object Level {
    case object Junior extends Level
    case object Mid extends Level
    case object Senior extends Level
  }

val programmers: List[Programmer] = List(
  Programmer(name= "Keylly",
             level= Level.Mid,
             knownLanguages= List("JS")),
  Programmer(name= "John",
             level= Level.Senior,
             knownLanguages= List("Java","Scala","Kotlin")),
  Programmer(name="Dave",
             level= Level.Junior,
             knownLanguages=List("c","c++"))
)
val isMidOrSenior:Programmer => Boolean =
  programmer => {
    println("verify level "+programmer)
    List(Level.Mid,Level.Senior).contains(programmer.level)
  }
val knowsMoreThan1Language:Programmer => Boolean =
  programmer => {
    println("verify number of known languages "+programmer)
    programmer.knownLanguages.size > 1
  }
val getName: Programmer => String =
  programmer => {
    println("get name " + programmer)
    programmer.name
  }

object filterWithFilterApp extends App{
  println("---------------It is filter---------------------------")
  val desiredProgrammers: List[Programmer] = programmers.filter(isMidOrSenior).filter(knowsMoreThan1Language)
  desiredProgrammers foreach getName
  println("---------------It is withFilter-----------------------")
  val anotherDesiredProgrammers =programmers.withFilter(isMidOrSenior).withFilter(knowsMoreThan1Language)
  anotherDesiredProgrammers foreach getName
  
}
