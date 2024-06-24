package zcore_003

import spinal.core._
import spinal.lib._

object ShiftOp extends SpinalEnum {
  val logicL, arithR, logicR = newElement()
}

case class Shifter() extends Component {
  val io = new Bundle {
    val a = in Bits (32 bits)
    val b = in Bits (5 bits)
    val shiftOp = in(ShiftOp())
    val result = out Bits (32 bits)
  }
  val results = Map(
    ShiftOp.arithR -> (io.a.asSInt >> io.b.asUInt),
    ShiftOp.logicL -> (io.a |<< io.b.asUInt),
    ShiftOp.logicR -> (io.a |>> io.b.asUInt)
  )
  switch(io.shiftOp) {
    for ((k, v) <- results) {
      is(k) { io.result := v.asBits }
    }
  }
}

object ShifterVerilog extends App {
  val report = Config.spinal.generateVerilog(Shifter())
  println(report.getRtlString())
}
