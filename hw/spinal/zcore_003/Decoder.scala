package zcore_003

import spinal.core._
import spinal.lib._

object CtrlSignals {
  object AluBSrc extends SpinalEnum {
    val rt, imm = newElement()
  }

  object WriteBackSrc extends SpinalEnum {
    val rd, rt, shamt = newElement()
  }

}

object InstDecoderInfo1 {
  import CtrlSignals._

  // Helper function to build the bundle.
  def apply(
      aluBSrc: AluBSrc.C,
      aluOp: AluOp.C,
      shiftOp: ShiftOp.C,
      writeBackSrc: WriteBackSrc.C
  ): InstDecoderInfo1 = {
    val ret = InstDecoderInfo1()

    ret.aluBSrc := aluBSrc
    ret.aluOp := aluOp
    ret.shiftOp := shiftOp
    ret.writeBackSrc := writeBackSrc

    ret
  }
}

case class InstDecoderInfo1() extends Bundle {
  import CtrlSignals._

  val aluBSrc = AluBSrc()
  val aluOp = AluOp()
  val shiftOp = ShiftOp()
  val writeBackSrc = WriteBackSrc()
}

object InstDecoderStage1 {
  import CtrlSignals._

  def apply(instType: InstType.C): InstDecoderInfo1 = {
    val ret = InstDecoderInfo1()
    val decoderMap = Map(
      InstType.addiu -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.add,
        ShiftOp.logicL,
        WriteBackSrc.rd
      )
    )

    switch(instType) {
      for ((k, v) <- decoderMap) {
        is(k) { ret := v }
      }
    }

    ret
  }
}

case class InstDecoderInfo0() extends Bundle {
  val instType = InstType()
  val op = Bits(6 bits)
  val rs = Bits(5 bits)
  val rt = Bits(5 bits)
  val rd = Bits(5 bits)
  val shamt = Bits(5 bits)
  val funct = Bits(6 bits)
  val imm = Bits(16 bits)
  val taddr = Bits(26 bits)
}

object InstDecoderStage0 {
  def apply(instruction: Bits): InstDecoderInfo0 = {
    require(instruction.getBitsWidth == 32)

    val ret = InstDecoderInfo0()

    ret.op := instruction(31 downto 26)
    ret.rs := instruction(25 downto 21)
    ret.rt := instruction(20 downto 16)
    ret.rd := instruction(15 downto 11)
    ret.shamt := instruction(10 downto 6)
    ret.funct := instruction(5 downto 0)
    ret.imm := instruction(15 downto 0)
    ret.taddr := instruction(25 downto 0)
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
