package zcore_003

import spinal.core._
import spinal.lib._

object AluOp extends SpinalEnum(defaultEncoding = binarySequential) {
  val and, or, add, sltu, xor, nor, sub, slt = newElement()
}

case class AluResult() extends Bundle {
  val overflow, carryOut, zero = Bool()
  val value = Bits(32 bits)
}

object Alu {
  def apply(a: Bits, b: Bits, aluOp: AluOp.C): AluResult = {
    val ret = AluResult()
    ret.carryOut := False
    switch(aluOp) {
      is(AluOp.and) {
        ret.value := a & b
      }
      is(AluOp.or) {
        ret.value := a | b
      }
      is(AluOp.add) {
        (ret.carryOut, ret.value) := a.asUInt +^ b.asUInt
      }
      is(AluOp.sltu) {
        when(a.asUInt < b.asUInt) {
          ret.value := 1
        } otherwise {
          ret.value := 0
        }
      }
      is(AluOp.xor) {
        ret.value := a ^ b
      }
      is(AluOp.nor) {
        ret.value := ~(a | b)
      }
      is(AluOp.sub) {
        (ret.carryOut, ret.value) := a.asUInt -^ b.asUInt
      }
      is(AluOp.slt) {
        when(a.asSInt < b.asSInt) {
          ret.value := 1
        } otherwise {
          ret.value := 0
        }
      }
    }
    ret.overflow := False
    when(aluOp === AluOp.add) {
      ret.overflow := (a.msb === b.msb) & (a.msb =/= ret.value.msb)
    } elsewhen (aluOp === AluOp.sub) {
      ret.overflow := (a.msb =/= b.msb) & (a.msb =/= ret.value.msb)
    }
    ret.zero := ret.value === 0
    ret
  }
}

object AluVerilog extends App {
  val report = Config.spinal.generateVerilog(new Component {
    val io = new Bundle {
      val a, b = in Bits (32 bits)
      val aluOp = in(AluOp())
      val overflow, carryOut, zero = out Bool ()
      val result = out Bits (32 bits)
    }
    (io.overflow, io.carryOut, io.zero, io.result) := Alu(io.a, io.b, io.aluOp)
  })
  println(report.getRtlString())
}
