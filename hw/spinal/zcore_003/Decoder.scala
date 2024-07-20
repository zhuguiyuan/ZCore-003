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
    val rd, rt, r31 = newElement()
  }

  object WriteBackDataSrc extends SpinalEnum {
    val alu, shifter, mem, nextPc = newElement()
  }

  object ImmExtType extends SpinalEnum {
    val zeroExt, signExt, luiExt = newElement()
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
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.subu -> InstDecoderInfo1(
        AluOp.sub,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.and -> InstDecoderInfo1(
        AluOp.and,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.or -> InstDecoderInfo1(
        AluOp.or,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.xor -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.nor -> InstDecoderInfo1(
        AluOp.nor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.slt -> InstDecoderInfo1(
        AluOp.slt,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.sltu -> InstDecoderInfo1(
        AluOp.slt,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.addiu -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.andi -> InstDecoderInfo1(
        AluOp.and,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.ori -> InstDecoderInfo1(
        AluOp.or,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.xori -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.slti -> InstDecoderInfo1(
        AluOp.slt,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.sltiu -> InstDecoderInfo1(
        AluOp.slt,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.sll -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.sra -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.arithR,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.srl -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicR,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.sllv -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.rs,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.srav -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.arithR,
        ShiftSrc.rs,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.srlv -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicR,
        ShiftSrc.rs,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.shifter,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.beq -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.bne -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.blez -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.bgtz -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.bltz -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.bgez -> InstDecoderInfo1(
        AluOp.xor,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.br
      ),
      InstType.j -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.j
      ),
      InstType.jal -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.r31,
        WriteBackDataSrc.nextPc,
        ImmExtType.zeroExt,
        PcSrc.j
      ),
      InstType.jr -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.alu
      ),
      InstType.jalr -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.r31,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.alu
      ),
      InstType.lb -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.lh -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.lw -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.lbu -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.lhu -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.lwl -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.lwr -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.mem,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.sb -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.sh -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.sw -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.swl -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.swr -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.no,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.signExt,
        PcSrc.p4
      ),
      InstType.movz -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.movn -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.rt,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rd,
        WriteBackDataSrc.alu,
        ImmExtType.zeroExt,
        PcSrc.p4
      ),
      InstType.lui -> InstDecoderInfo1(
        AluOp.add,
        AluBSrc.imm,
        ShiftOp.logicL,
        ShiftSrc.sa,
        WriteBackEn.yes,
        WriteBackRegSrc.rt,
        WriteBackDataSrc.alu,
        ImmExtType.luiExt,
        PcSrc.p4
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
      M"00000000000---------------000011" -> InstType.sra,
      M"00000000000---------------000010" -> InstType.srl,
      M"000000---------------00000000100" -> InstType.sllv,
      M"000000---------------00000000111" -> InstType.srav,
      M"000000---------------00000000110" -> InstType.srlv,
      M"000100--------------------------" -> InstType.beq,
      M"000101-----00000----------------" -> InstType.bne,
      M"000110--------------------------" -> InstType.blez,
      M"000111-----00000----------------" -> InstType.bgtz,
      M"000001-----00000----------------" -> InstType.bltz,
      M"000001-----00001----------------" -> InstType.bgez,
      M"000010--------------------------" -> InstType.j,
      M"000011--------------------------" -> InstType.jal,
      M"000000-----000000000000000001000" -> InstType.jr,
      M"000000-----00000-----00000001001" -> InstType.jalr,
      M"100000--------------------------" -> InstType.lb,
      M"100001--------------------------" -> InstType.lh,
      M"100011--------------------------" -> InstType.lw,
      M"100100--------------------------" -> InstType.lbu,
      M"100101--------------------------" -> InstType.lhu,
      M"100010--------------------------" -> InstType.lwl,
      M"100100--------------------------" -> InstType.lwr,
      M"101000--------------------------" -> InstType.sb,
      M"101001--------------------------" -> InstType.sh,
      M"101011--------------------------" -> InstType.sw,
      M"101010--------------------------" -> InstType.swl,
      M"101110--------------------------" -> InstType.swr,
      M"000000---------------00000001010" -> InstType.movz,
      M"000000---------------00000001011" -> InstType.movn,
      M"001111--------------------------" -> InstType.lui
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
