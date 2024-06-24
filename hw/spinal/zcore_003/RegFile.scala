package zcore_003

import spinal.core._
import spinal.lib._

case class RegFile() extends Component {
  val io = new Bundle {
    val waddr, raddr1, raddr2 = in UInt (5 bits)
    val wen = in Bool ()
    val wdata = in Bits (32 bits)
    val rdata1, rdata2 = out Bits (32 bits)
  }
  noIoPrefix()

  val registers = Vec.fill(32)(Reg(Bits(32 bits)))
  when(io.wen) {
    registers(io.waddr) := io.wdata
  }

  io.rdata1 := registers(io.raddr1)
  when(io.raddr1 === 0) {
    io.rdata1 := 0
  }

  io.rdata2 := registers(io.raddr2)
  when(io.raddr2 === 0) {
    io.rdata2 := 0
  }
}
