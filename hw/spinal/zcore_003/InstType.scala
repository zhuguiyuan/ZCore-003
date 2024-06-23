package zcore_003

import spinal.core._
import spinal.lib._

object InstType extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val addu, subu, and, nor, or, xor, slt, sltu = newElement()
  val addiu, andi, ori, xori, slti, sltiu = newEmelent()
  val sll, sllv, sra, srav, srl, srlv = newElement()
  val bne, beq, bgez, bgtz, blez, bltz, j, jal, jr, jalr = newElement()
  val lb, lh, lw, lbu, lhu, lwl, ler, sb, sh, sw, swl, swa = newElement()
  val movn, movz, lui = newElement()
  val invalid = newElement()
}
