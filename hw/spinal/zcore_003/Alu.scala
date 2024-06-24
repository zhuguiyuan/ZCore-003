package zcore_003

import spinal.core._
import spinal.lib._

object AluOp extends SpinalEnum(defaultEncoding = binarySequential) {
  val and, or, add, sltu, xor, nor, sub, slt = newElement()
}

case class Alu() extends Component {
  val io = new Bundle {
    val a, b = in Bits (32 bits)
    val aluOp = in(AluOp())
    val overflow, carryOut, zero = out Bool ()
    val result = out Bits (32 bits)
  }
  noIoPrefix()

  io.carryOut := False
  switch(io.aluOp) {
    is(AluOp.and) {
      io.result := io.a & io.b
    }
    is(AluOp.or) {
      io.result := io.a | io.b
    }
    is(AluOp.add) {
      (io.carryOut, io.result) := io.a.asUInt +^ io.b.asUInt
    }
    is(AluOp.sltu) {
      when(io.a.asUInt < io.b.asUInt) {
        io.result := 1
      } otherwise {
        io.result := 0
      }
    }
    is(AluOp.xor) {
      io.result := io.a ^ io.b
    }
    is(AluOp.nor) {
      io.result := ~(io.a | io.b)
    }
    is(AluOp.sub) {
      (io.carryOut, io.result) := io.a.asUInt -^ io.b.asUInt
    }
    is(AluOp.slt) {
      when(io.a.asSInt < io.b.asSInt) {
        io.result := 1
      } otherwise {
        io.result := 0
      }
    }
  }

  io.overflow := False
  when(io.aluOp === AluOp.add) {
    io.overflow := (io.a.msb === io.b.msb) & (io.a.msb =/= io.result.msb)
  } elsewhen (io.aluOp === AluOp.sub) {
    io.overflow := (io.a.msb =/= io.b.msb) & (io.a.msb =/= io.result.msb)
  }

  io.zero := io.result === 0
}

object AluVerilog extends App {
  val report = Config.spinal.generateVerilog(Alu())
  println(report.getRtlString())
}
