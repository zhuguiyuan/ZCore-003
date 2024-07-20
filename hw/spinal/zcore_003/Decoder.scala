package zcore_003

import spinal.core._
import spinal.lib._

object CtrlSignals {
  object AluBSrc extends SpinalEnum {
    val rt, imm = newElement()
  }

  object ShiftSrc extends SpinalEnum {
    val sa, rs = newElement()
  }

  object WriteBackEn extends SpinalEnum {
    val yes, no = newElement()
  }

  object WriteBackRegSrc extends SpinalEnum {
    val rd, rt = newElement()
  }

  object WriteBackDataSrc extends SpinalEnum {
    val alu, shifter, mem, nextPc = newElement()
  }

  object ImmExtType extends SpinalEnum {
    val zeroExt, signExt = newElement()
  }

  object PcSrc extends SpinalEnum {
    val p4, br, j, alu = newElement()
  }
}

object InstDecoderInfo1 {
  import CtrlSignals._

  // Helper function to build the bundle.
  def apply(
      aluOp: AluOp.C,
      aluBSrc: AluBSrc.C,
      shiftOp: ShiftOp.C,
      shiftSrc: ShiftSrc.C,
      writeBackEn: WriteBackEn.C,
      writeBackRegSrc: WriteBackRegSrc.C,
      writeBackDataSrc: WriteBackDataSrc.C,
      immExtType: ImmExtType.C,
      pcSrc: PcSrc.C
  ): InstDecoderInfo1 = {
    val ret = InstDecoderInfo1()

    ret.aluOp := aluOp
    ret.aluBSrc := aluBSrc
    ret.shiftOp := shiftOp
    ret.shiftSrc := shiftSrc
    ret.writeBackEn := writeBackEn
    ret.writeBackRegSrc := writeBackRegSrc
    ret.writeBackDataSrc := writeBackDataSrc
    ret.immExtType := immExtType
    ret.pcSrc := pcSrc

    ret
  }
}

case class InstDecoderInfo1() extends Bundle {
  import CtrlSignals._

  val aluOp = AluOp()
  val aluBSrc = AluBSrc()
  val shiftOp = ShiftOp()
  val shiftSrc = ShiftSrc()
  val writeBackEn = WriteBackEn()
  val writeBackRegSrc = WriteBackRegSrc()
  val writeBackDataSrc = WriteBackDataSrc()
  val immExtType = ImmExtType()
  val pcSrc = PcSrc()
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
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.subu -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.sub,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.and -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.and,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.or -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.or,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.xor -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.xor,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.nor -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.nor,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.slt -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.slt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.sltu -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.sltu,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.addiu -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.add,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt
      ),
      InstType.andi -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.and,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.ori -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.or,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.xori -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.xor,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt
      ),
      InstType.slti -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.slt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt
      ),
      InstType.sltiu -> InstDecoderInfo1(
        AluBSrc.imm,
        AluOp.sltu,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt
      ),
      InstType.sll -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      ),
      InstType.sllv -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.logicL,
        ShiftSrc.rs,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      ),
      InstType.sra -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.arithR,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      ),
      InstType.srav -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.arithR,
        ShiftSrc.rs,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      ),
      InstType.srl -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.logicR,
        ShiftSrc.sa,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      ),
      InstType.srlv -> InstDecoderInfo1(
        AluBSrc.rt,
        AluOp.add,
        ShiftOp.logicR,
        ShiftSrc.rs,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.signExt
      )
    )
    val nopCtrl = InstDecoderInfo1(
      AluOp.add,
      AluBSrc.rt,
      ShiftOp.logicL,
      ShiftSrc.sa,
      WriteBackEn.no,
      WriteBackRegSrc.rt,
      WriteBackDataSrc.alu,
      ImmExtType.zeroExt,
      PcSrc.p4
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
  val rs = UInt(5 bits)
  val rt = UInt(5 bits)
  val rd = UInt(5 bits)
  val shamt = Bits(5 bits)
  val funct = Bits(6 bits)
  val imm = Bits(16 bits)
  val taddr = UInt(26 bits)
}

object InstDecoderStage0 {
  def apply(instruction: Bits): InstDecoderInfo0 = {
    require(instruction.getBitsWidth == 32)

    val ret = InstDecoderInfo0()

    ret.op := instruction(31 downto 26)
    ret.rs := instruction(25 downto 21).asUInt
    ret.rt := instruction(20 downto 16).asUInt
    ret.rd := instruction(15 downto 11).asUInt
    ret.shamt := instruction(10 downto 6)
    ret.funct := instruction(5 downto 0)
    ret.imm := instruction(15 downto 0)
    ret.taddr := instruction(25 downto 0).asUInt
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
      M"001011--------------------------" -> InstType.sltiu,
      M"00000000000---------------000000" -> InstType.sll,
      M"00000000000---------------000100" -> InstType.sllv,
      M"00000000000---------------000011" -> InstType.sra,
      M"00000000000---------------000111" -> InstType.srav,
      M"00000000000---------------000010" -> InstType.srl,
      M"00000000000---------------000110" -> InstType.srlv
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
