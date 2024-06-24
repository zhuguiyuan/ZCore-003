package zcore_003

import spinal.core._
import spinal.lib._

object RegFile {
  def apply(waddr: UInt, raddr1: UInt, raddr2: UInt, wen: Bool, wdata: Bits) = {
    require(waddr.getBitsWidth == 5)
    require(raddr1.getBitsWidth == 5)
    require(raddr2.getBitsWidth == 5)
    require(wdata.getBitsWidth == 32)

    val ret = new Bundle {
      val rdata1, rdata2 = Bits(32 bits)
    }

    val content = new RegFile()
    content.io.waddr <> waddr
    content.io.raddr1 <> raddr1
    content.io.raddr2 <> raddr2
    content.io.wen <> wen
    content.io.rdata1 <> ret.rdata1
    content.io.rdata2 <> ret.rdata2

    ret
  }
}

class RegFile() extends Component {
  val io = new Bundle {
    val waddr, raddr1, raddr2 = in UInt (5 bits)
    val wen = in Bool ()
    val wdata = in Bits (32 bits)
    val rdata1, rdata2 = out Bits (32 bits)
  }

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
