import ScalaProgrammingV4.Chapter4.ChecksumAccumulator.calculate

object Summer{
  def main(args:Array[String])={
    for(arg <- args){
      println(arg + ": " + calculate(arg))
    }
  }
}
