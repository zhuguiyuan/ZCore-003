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

  object ExtType extends SpinalEnum {
    val zeroExt, signExt = newElement()
  }
}

object InstDecoderInfo1 {
  import CtrlSignals._

  // Helper function to build the bundle.
  def apply(
      aluBSrc: AluBSrc.C,
      aluOp: AluOp.C,
      shiftOp: ShiftOp.C,
      writeBackSrc: WriteBackSrc.C,
      extType: ExtType.C
  ): InstDecoderInfo1 = {
    val ret = InstDecoderInfo1()

    ret.aluBSrc := aluBSrc
    ret.aluOp := aluOp
    ret.shiftOp := shiftOp
    ret.writeBackSrc := writeBackSrc
    ret.extType := extType

    ret
  }
}

case class InstDecoderInfo1() extends Bundle {
  import CtrlSignals._

  val aluBSrc = AluBSrc()
  val aluOp = AluOp()
  val shiftOp = ShiftOp()
  val writeBackSrc = WriteBackSrc()
  val extType = ExtType()
}

object InstDecoderStage1 {
  import CtrlSignals._

  def apply(instType: InstType.C): InstDecoderInfo1 = {
    val ret = InstDecoderInfo1()
    val decoderMap = Map(
      InstType.addu -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.subu -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.sub,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.and -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.and,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.or -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.or,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.xor -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.xor,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.nor -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.nor,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.slt -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.slt,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.sltu -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.sltu,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.addiu -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.add,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.signExt
      ),
      InstType.andi -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.and,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.ori -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.or,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.xori -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.xor,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.zeroExt
      ),
      InstType.slti -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.slt,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.signExt
      ),
      InstType.sltiu -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.sltu,
        ShiftOp.logicL,
        WriteBackSrc.rd,
        ExtType.signExt
      )
    )
    val nopCtrl = InstDecoderInfo1(
      AluBSrc.rt,
      AluOp.add,
      ShiftOp.logicL,
      WriteBackSrc.rt,
      ExtType.zeroExt
    )

    switch(instType) {
      for ((k, v) <- decoderMap) {
        is(k) { ret := v }
      }
      default { ret := nopCtrl }
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
      M"000000---------------00000100001" -> InstType.addu,
      M"000000---------------00000100011" -> InstType.subu,
      M"000000---------------00000100100" -> InstType.and,
      M"000000---------------00000100101" -> InstType.or,
      M"000000---------------00000100110" -> InstType.xor,
      M"000000---------------00000100111" -> InstType.nor,
      M"000000---------------00000101010" -> InstType.slt,
      M"000000---------------00000101011" -> InstType.sltu,
      M"001001--------------------------" -> InstType.addiu,
      M"001100--------------------------" -> InstType.andi,
      M"001101--------------------------" -> InstType.ori,
      M"001110--------------------------" -> InstType.xori,
      M"001010--------------------------" -> InstType.slti,
      M"001011--------------------------" -> InstType.sltiu
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
