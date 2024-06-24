package zcore_003

import spinal.core._
import spinal.lib._

object ShiftOp extends SpinalEnum {
  val logicL, arithR, logicR = newElement()
}

object Shifter {
  def apply(a: Bits, b: Bits, shiftOp: ShiftOp.C): Bits = {
    require(a.getBitsWidth == 32)
    require(b.getBitsWidth == 5)

    val ret = Bits(32 bits)

    val results = Map(
      ShiftOp.arithR -> (a.asSInt >> b.asUInt),
      ShiftOp.logicL -> (a |<< b.asUInt),
      ShiftOp.logicR -> (a |>> b.asUInt)
    )
    switch(shiftOp) {
      for ((k, v) <- results) {
        is(k) { ret := v.asBits }
      }
    }

    ret
  }
}
