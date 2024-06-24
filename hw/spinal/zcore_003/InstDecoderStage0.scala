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
    val content = new InstDecoderStage0()
    content.io.instruction <> instruction
    content.io.instInfo
  }
}

class InstDecoderStage0() extends Component {
  val io = new Bundle {
    val instruction = in Bits (32 bits)
    val instInfo = out(InstDecoderInfo0())
  }

  io.instInfo.instOp := io.instruction(31 downto 26)
  io.instInfo.instRs := io.instruction(25 downto 21)
  io.instInfo.instRt := io.instruction(20 downto 16)
  io.instInfo.instRd := io.instruction(15 downto 11)
  io.instInfo.instShamt := io.instruction(10 downto 6)
  io.instInfo.instFunct := io.instruction(5 downto 0)
  io.instInfo.instImm := io.instruction(15 downto 0)
  io.instInfo.instTAddr := io.instruction(25 downto 0)
  val decodingMap = Map(
    M"001001--------------------------" -> InstType.addiu
  )
  switch(io.instruction) {
    for ((k, v) <- decodingMap) {
      is(k) { io.instInfo.instType := v }
    }
    default -> { io.instInfo.instType := InstType.invalid }
  }
}

object InstDecoderStage0Verilog extends App {
  val report = Config.spinal.generateVerilog(new InstDecoderStage0())
  println(report.getRtlString())
}
