package zcore_003

import spinal.core._
import spinal.lib._

case class InstDecoderInfo0() extends Bundle {
  val instType = InstType()
  val instOp = Bits(6 bits)
  val instRs = Bits(5 bits)
  val instRt = Bits(5 bits)
  val instRd = Bits(5 bits)
  val instShamt = Bits(5 bits)
  val instFunct = Bits(6 bits)
  val instImm = Bits(16 bits)
  val instTAddr = Bits(26 bits)
}

object InstDecoderStage0 {
  def apply(instruction: Bits): InstDecoderInfo0 = {
    require(instruction.getBitsWidth == 32)

    val ret = InstDecoderInfo0()

    ret.instOp := instruction(31 downto 26)
    ret.instRs := instruction(25 downto 21)
    ret.instRt := instruction(20 downto 16)
    ret.instRd := instruction(15 downto 11)
    ret.instShamt := instruction(10 downto 6)
    ret.instFunct := instruction(5 downto 0)
    ret.instImm := instruction(15 downto 0)
    ret.instTAddr := instruction(25 downto 0)
    val decodingMap = Map(
      M"001001--------------------------" -> InstType.addiu
    )
    switch(instruction) {
      for ((k, v) <- decodingMap) {
        is(k) { ret.instType := v }
      }
      default -> { ret.instType := InstType.invalid }
    }

    ret
  }
}
