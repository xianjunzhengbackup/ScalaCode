package simulation

object MySimulation extends CircuitSimulation {
  override def InverterDelay: Int = 1

  override def AndGateDelay: Int = 3

  override def OrGateDelay: Int = 5

  def main(args:Array[String])={

//    val input1, input2, sum, carry = new Wire
//
//    probe("sum", sum)
//    probe("carry", carry)
//
//    val h = halfAdder(input1,input2,sum,carry)
//    input1 setSignal true
//    run()
//
//    input2 setSignal true
//    run()
//
    val a1,a2,a3,a4 = new Wire
    inverter(a1,a2)
    inverter(a2,a3)
    inverter(a3,a4)

    probe("a1",a1)
    probe("a2",a2)
    probe("a3",a3)
    probe("a4",a4)

    println("Before setting values")
    run()


    a1 setSignal true
    println("set a1 to true")
    run()
  }


}
