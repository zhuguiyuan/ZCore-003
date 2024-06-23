package zcore_003

import spinal.core._
import spinal.lib._

case class InstDecoderStage0() {
  val io = new Bundle {
    val instruction = in Bits (32 bits)

    val instType = out(InstType())
    val instOp = out Bits (6 bits)
    val instRs = out Bits (5 bits)
    val instRt = out Bits (5 bits)
    val instRd = out Bits (5 bits)
    val instShamt = out Bits (5 bits)
    val instFunct = out Bits (6 bits)
    val instImm = out Bits (16 bits)
    val instTAddr = out Bits (26 bits)
  }

  io.instOp := io.instruction(31 downto 26)
  io.instRs := io.instruction(25 downto 21)
  io.instRt := io.instruction(20 downto 16)
  io.instRd := io.instruction(15 downto 11)
  io.instShamt := io.instruction(10 downto 6)
  io.instFunct := io.instruction(5 downto 0)
  io.instImm := io.instruction(15 downto 0)
  io.instTAddr := io.instruction(25 downto 0)
  io.instType := io.instruction.mux(
    M"001001--------------------------" -> InstType.addiu,
    default -> InstType.invalid
  )

}
