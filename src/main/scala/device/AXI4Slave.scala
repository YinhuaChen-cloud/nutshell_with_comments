/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package device

import chisel3._
import chisel3.util._

import nutcore.HasNutCoreParameter
import bus.axi4._
import utils._

// 这里的中括号用于指定泛型类型参数。泛型类型参数是一种在类或方法定义中使用的占位符类型，它们可以让你在使用类或方法时指定具体的类型。
// [T <: AXI4Lite] 表示 T是AXI4Lite类型、或其子类型
// [B <: AXI4Lite] 表示 T是Data类型、或其子类型
abstract class AXI4SlaveModule[T <: AXI4Lite, B <: Data](_type :T = new AXI4, _extra: B = null) // 默认第一个参数是一个AXI4 Bundle, 第二个参数是 null
  extends Module with HasNutCoreParameter {
  val io = IO(new Bundle{
    val in = Flipped(_type) // AXI4 和 AXI4Lite 默认是用在主模块上的，这里我们要设计 Slave 端，所以要 Flipped
    val extra = if (_extra != null) Some(Flipped(Flipped(_extra))) else None // extra 似乎是用来作为 AXI4 总线的补充，由于要跟外面对接，所以也要 Flipped
    // 在Chisel中，Some类型常常用于描述需要在运行时确定的类型。 常用的方法为 getOrElse(xxx)，当Some类型中没有所需的内容时，返回xxx
  })
  val in = io.in // 给 io.in 起个别名叫 in      AXI4类型Bundle

  val fullMask = MaskExpand(in.w.bits.strb) // 从小 strb 获取 fullMask
  def genWdata(originData: UInt) = (originData & ~fullMask) | (in.w.bits.data & fullMask) // 参数 originData 用来表示“原数据”，新数据在和掩码做"与"处理后，要和元数据做“或”运算

  val raddr = Wire(UInt()) // 读地址
  val ren = Wire(Bool()) // 读使能
  val (readBeatCnt, rLast) = in match { // 这是一个模式匹配语句
    case axi4: AXI4 => // 如果 in 是 AXI4 类型
      val c = Counter(256)
      val beatCnt = Counter(256)
      val len = HoldUnless(axi4.ar.bits.len, axi4.ar.fire())
      val burst = HoldUnless(axi4.ar.bits.burst, axi4.ar.fire())
      val wrapAddr = axi4.ar.bits.addr & ~(axi4.ar.bits.len.asTypeOf(UInt(PAddrBits.W)) << axi4.ar.bits.size)
      raddr := HoldUnless(wrapAddr, axi4.ar.fire())
      axi4.r.bits.last := (c.value === len)
      when (ren) {
        beatCnt.inc()
        when (burst === AXI4Parameters.BURST_WRAP && beatCnt.value === len) { beatCnt.value := 0.U }
      }
      when (axi4.r.fire()) {
        c.inc()
        when (axi4.r.bits.last) { c.value := 0.U }
      }
      when (axi4.ar.fire()) {
        beatCnt.value := (axi4.ar.bits.addr >> axi4.ar.bits.size) & axi4.ar.bits.len
        when (axi4.ar.bits.len =/= 0.U && axi4.ar.bits.burst === AXI4Parameters.BURST_WRAP) {
          assert(axi4.ar.bits.len === 1.U || axi4.ar.bits.len === 3.U ||
            axi4.ar.bits.len === 7.U || axi4.ar.bits.len === 15.U)
        }
      }
      (beatCnt.value, axi4.r.bits.last)

    case axi4lite: AXI4Lite => // 如果 in 是一个 AXI4Lite 类型
      raddr := axi4lite.ar.bits.addr // 那么 raddr(读地址) 就是 in.ar.bits.addr
      (0.U, true.B) // readBeatCnt 为 0， rLast 为 true.B          readBeatCnt 似乎是一个只在AXI4下用的东西     RLAST ：标记最后一次读数据传输
  }

  // 读忙碌，猜测：用来表示读通道在忙？
  val r_busy = BoolStopWatch(in.ar.fire(), in.r.fire() && rLast, startHighPriority = true) 
  in.ar.ready := in.r.ready || !r_busy
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  ren := RegNext(in.ar.fire(), init=false.B) || (in.r.fire() && !rLast)
  in.r.valid := BoolStopWatch(ren && (in.ar.fire() || r_busy), in.r.fire(), startHighPriority = true)


  val waddr = Wire(UInt())
  val (writeBeatCnt, wLast) = in match {
    case axi4: AXI4 =>
      val c = Counter(256)
      waddr := HoldUnless(axi4.aw.bits.addr, axi4.aw.fire())
      when (axi4.w.fire()) {
        c.inc()
        when (axi4.w.bits.last) { c.value := 0.U }
      }
      (c.value, axi4.w.bits.last)

    case axi4lite: AXI4Lite =>
      waddr := axi4lite.aw.bits.addr
      (0.U, true.B)
  }

  val w_busy = BoolStopWatch(in.aw.fire(), in.b.fire(), startHighPriority = true)
  in.aw.ready := !w_busy
  in. w.ready := in.aw.valid || (w_busy)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
  in.b.valid := BoolStopWatch(in.w.fire() && wLast, in.b.fire(), startHighPriority = true)

  in match {
    case axi4: AXI4 =>
      axi4.b.bits.id   := RegEnable(axi4.aw.bits.id, axi4.aw.fire())
      axi4.b.bits.user := RegEnable(axi4.aw.bits.user, axi4.aw.fire())
      axi4.r.bits.id   := RegEnable(axi4.ar.bits.id, axi4.ar.fire())
      axi4.r.bits.user := RegEnable(axi4.ar.bits.user, axi4.ar.fire())
    case axi4lite: AXI4Lite =>
  }
}
