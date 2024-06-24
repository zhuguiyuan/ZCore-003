package zcore_003

import spinal.core._
import spinal.lib._

object AluOp {
  def and = B"000"
  def or = B"001"
  def add = B"010"
  def sltu = B"011"
  def xor = B"100"
  def nor = B"101"
  def sub = B"110"
  def slt = B"111"
}

case class Alu() extends Component {
  setName("ALU")

  val io = new Bundle {
    val a, b = in Bits (32 bits)
    val aluOp = in Bits (3 bits)
    val overflow, carryOut, zero = out Bool ()
    val result = out Bits (32 bits)

    a.setName("A")
    b.setName("B")
    overflow.setName("Overflow")
    carryOut.setName("CarryOut")
    zero.setName("Zero")
    result.setName("Result")
  }

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
