package ScalaProgrammingV4.Chapter18

abstract class CircuitSimulation extends BasicCircuitSimulation{
  def halfAdder(a: Wire,b: Wire,s: Wire,c: Wire)={
    val d,e = new Wire
    orGate(a,b,d)
    andGate(a,b,c)
    inverter(c,e)
    andGate(d,e,s)
  }
  def fullAdder(a: Wire,b: Wire,cin: Wire,sum: Wire,count: Wire)={
    val s,c1,c2 = new Wire
    halfAdder(a,cin,s,c1)
    halfAdder(b,s,sum,c2)
    orGate(c1,c2,count)
  }
}

object CircuitSimulationApp extends App{
  object MySimulation extends CircuitSimulation{
    def InverterDelay = 1
    def AndGateDelay = 3
    def OrGateDelay =5

    val input1,input2,sum,carry,count = new Wire
    val adder= fullAdder(input1,input2,carry,sum,count)
    probe("sum",sum)
    probe("carry",count)

    input1.setSignal(true)
    input2.setSignal(true)
  }
  MySimulation.run()
}

