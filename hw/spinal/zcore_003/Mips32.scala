package zcore_003

import spinal.core._
import spinal.lib._

case class Mips32() extends Component {
  // to match the course verilog skeleton
  setName("simple_cpu")

  val io = new Bundle {
    val pc = out UInt (32 bits)
    val instruction = in Bits (32 bits)
    val address = out UInt (32 bits)
    val memWrite = out Bool ()
    val writeData = out Bits (32 bits)
    val writeStrb = out Bits (3 bits)
    val readData = in Bits (32 bits)
    val memRead = out Bool ()

    // to match the course verilog skeleton
    pc.setName("PC")
    instruction.setName("Instruction")
    address.setName("Address")
    memWrite.setName("MemWrite")
    writeData.setName("Write_data")
    writeStrb.setName("Write_strb")
    readData.setName("Read_data")
    memRead.setName("MemRead")
  }

  // some fixed signals for test from the course spec
  val rf_wen = Bool()
  val rf_waddr = UInt(5 bits)
  val rf_wdata = Bits(32 bits)
  rf_wen.setName("RF_wen")
  rf_waddr.setName("RF_waddr")
  rf_wdata.setName("RF_data")

  import CtrlSignals._

  val instInfo0 = InstDecoderStage0(io.instruction)
  val instInfo1 = InstDecoderStage1(instInfo0.instType)
  val aluSrcA, aluSrcB = Bits(32 bits)
  val writeBackRegSrc = UInt(5 bits)
  val writeBackDataSrc = Bits(32 bits)
  val shiftSrc = Bits(5 bits)

  val pc = Reg(UInt(32 bits))
  val aluResult = Alu(aluSrcA, aluSrcB, instInfo1.aluOp)
  val extImm = Bits(32 bits)
  val regFile = RegFile(
    writeBackRegSrc,
    instInfo0.rs,
    instInfo0.rt,
    True,
    writeBackDataSrc
  )
  val shifter = Shifter(regFile.rdata2, shiftSrc, instInfo1.shiftOp)
  pc := pc + 4

  extImm := instInfo1.immExtType.mux(
    ImmExtType.zeroExt -> instInfo0.imm.resize(32 bits),
    ImmExtType.signExt -> instInfo0.imm.asSInt.resize(32 bits).asBits
  )
  aluSrcB := instInfo1.aluBSrc.mux(
    AluBSrc.rt -> regFile.rdata2,
    AluBSrc.imm -> extImm
  )
  writeBackRegSrc := instInfo1.writeBackRegSrc.mux(
    WriteBackRegSrc.rd -> instInfo0.rd,
    WriteBackRegSrc.rt -> instInfo0.rt
  )
  shiftSrc := instInfo1.shiftSrc.mux(
    ShiftSrc.sa -> instInfo0.shamt,
    ShiftSrc.rs -> regFile.rdata1(4 downto 0)
  )
  writeBackDataSrc := instInfo1.writeBackDataSrc.mux(
    WriteBackDataSrc.alu -> aluResult.value,
    WriteBackDataSrc.shifter -> shifter.value
  )

  io.pc := pc
  io.address := 0
  io.memWrite := False
  io.writeData := 0
  io.writeStrb := 0
  io.memRead := False
}

object Mips32Verilog extends App {
  val report = Config.spinal.generateVerilog(Mips32())
  println(report.getRtlString())
}
