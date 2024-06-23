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

}

object Mips32Verilog extends App {
  val report = Config.spinal.generateVerilog(Mips32())
  println(report.getRtlString())
}