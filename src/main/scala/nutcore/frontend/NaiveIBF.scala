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

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

// 1-width Naive Instruction Align Buffer
class NaiveRVCAlignBuffer extends NutCoreModule with HasInstrType with HasExceptionNO {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CtrlFlowIO))
    val out = Decoupled(new CtrlFlowIO)
    val flush = Input(Bool())
  })

  val instr = Wire(UInt(32.W))
  val isRVC = instr(1,0) =/= "b11".U

  //RVC support FSM
  //only ensure pnpc given by this FSM is right. May need flush after 6 offset 32 bit inst
  val s_idle :: s_extra :: s_waitnext :: s_waitnext_thenj :: Nil = Enum(4) 
  /*
    s_idle：正常状态
    s_extra: 取RVC指令
    s_waitnext: 所取指令是32bits，且跨了instLine，需要访问两次Cache才能取出来，指令不为jump类型
    s_waitnext_thenj: 所取指令是32bits，且跨了instLine，需要访问两次Cache才能取出来，指令为jump类型
  */
  val state = RegInit(UInt(2.W), s_idle)
  val pcOffsetR = RegInit(UInt(3.W), 0.U)
  val pcOffset = Mux(state === s_idle, io.in.bits.pc(2,0), pcOffsetR)
  val instIn = Cat(0.U(16.W), io.in.bits.instr)
  // val nextState = WireInit(0.U(2.W))
  val canGo = WireInit(false.B)
  val canIn = WireInit(false.B)
  // brIdx(0) -> branch at pc offset 0 (mod 4)
  // brIdx(1) -> branch at pc offset 2 (mod 4)
  // brIdx(2) -> branch at pc offset 6 (mod 8), and this inst is not rvc inst
  val brIdx = io.in.bits.brIdx
  // val brIdx = 0.U
  /*
    rvcFinish ：下一条指令位于4B对齐的地方
    case(pcOffset)
    0: !isRVC || brIdx(0) //当前指令不是RVC或者下条指令是跳转指令
    2: isRVC || brIdx(1) //当前指令是RVC或者下条指令是跳转指令
    4: !isRVC || brIdx(0) //当前指令不是RVC或者下条指令是跳转指令
    6: isRVC //当前指令是RVC
    endcase
  */
  val rvcFinish = pcOffset === 0.U && (!isRVC || brIdx(0)) || pcOffset === 4.U && (!isRVC || brIdx(0)) || pcOffset === 2.U && (isRVC || brIdx(1)) || pcOffset === 6.U && isRVC  
  // if brIdx(0) (branch taken at inst with offest 0), ignore the rest part of this instline
  // just get next pc and instline from IFU
  /*
    rvcNext ：下一条指令需要特殊处理（由RVC引入的未对齐）
    case(pcOffset)
    0: isRVC && !brIdx(0)
    2: !isRVC && !brIdx(1)
    4: isRVC && !brIdx(0)
    6: 0
    endcase
  */
  val rvcNext = pcOffset === 0.U && (isRVC && !brIdx(0)) || pcOffset === 4.U && (isRVC && !brIdx(0)) || pcOffset === 2.U && !isRVC && !brIdx(1)
  
  val rvcSpecial = pcOffset === 6.U && !isRVC && !brIdx(2)    /* 下条指令跨行，需要拼接，并且不是跳转指令 */
  val rvcSpecialJump = pcOffset === 6.U && !isRVC && brIdx(2) /* 下条指令跨行，需要拼接，并且是跳转指令 */
  val pnpcIsSeq = brIdx(3) /*下一条PC是不是顺序的*/
  // val pnpcIsSeqRight = io.in.bits.pnpc === (Cat(io.in.bits.pc(VAddrBits-1,2), 0.U(2.W)) + 4.U) // TODO: add a new user bit bpRight to do this 
  // assert(pnpcIsSeq === pnpcIsSeqRight)
  val flushIFU = (state === s_idle || state === s_extra) && rvcSpecial && io.in.valid && !pnpcIsSeq
  Debug(flushIFU, "flushIFU at pc %x offset %x\n", io.in.bits.pc, pcOffset)
  assert(!flushIFU)
  /* 需要跨行拼接指令 */
  val loadNextInstline = (state === s_idle || state === s_extra) && (rvcSpecial || rvcSpecialJump) && io.in.valid && pnpcIsSeq
  // val loadNextInstline =false.B
  val pcOut = WireInit(0.U(VAddrBits.W))
  val pnpcOut = WireInit(0.U(VAddrBits.W))
  val specialPCR = Reg(UInt(VAddrBits.W)) // reg for full inst that cross 2 inst line
  val specialNPCR = Reg(UInt(VAddrBits.W)) // reg for pnc for full inst jump that cross 2 inst line
  val specialInstR = Reg(UInt(16.W)) /* 存储跨InstLine指令的低16位 */
  val specialIPFR = RegInit(Bool(), false.B)
  val redirectPC = Cat(io.in.bits.pc(VAddrBits-1,3), 0.U(3.W))+"b1010".U // IDU can got get full inst from a single inst line  
  val rvcForceLoadNext = (pcOffset === 2.U && !isRVC && io.in.bits.pnpc(2,0) === 4.U && !brIdx(1))
  //------------------------------------------------------
  // rvcForceLoadNext is used to deal with: 
  // case 1:
  // 8010004a:	406007b7          	lui	a5,0x40600
  // 8010004e:	470d                	li	a4,3
  // 80100050:	00e78623          	sb	a4,12(a5) # 4060000c <_start-0x3faffff4>
  // For icache req inst in seq, if there is no rvcForceLoadNext, 
  // after 8010004e there will be 8010004c instead of 80100050
  //------------------------------------------------------
  // case 2:
  // 80100046:	406007b7          	lui	a5,0x40600
  // 8010004a:	470d              	li	a4,3
  // force load next instline into ID stage, if bp wrong, it will be flushed by flushIFU
  //------------------------------------------------------
  // if there is a j inst in current inst line, a redirect req will be sent by ALU before invalid inst exception being committed
  // when brIdx(1), next instline will just be branch target, eatline is no longer needed 

  // only for test, add this to pipeline when do real implementation
  // val predictBranch = io.in.valid && Mux(io.in.bits.pc(1), io.in.bits.pc + 2.U === io.in.bits.pnpc, io.in.bits.pc + 4.U === io.in.bits.pnpc)
  // val flush = rvcSpecial
  instr := Mux((state === s_waitnext || state === s_waitnext_thenj), Cat(instIn(15,0), specialInstR), LookupTree(pcOffset, List(
    "b000".U -> instIn(31,0),
    "b010".U -> instIn(31+16,16),
    "b100".U -> instIn(63,32),
    "b110".U -> instIn(63+16,32+16)
  )))

  when(!io.flush){
    switch(state){
      is(s_idle){//decode current pc in pipeline
        canGo := rvcFinish || rvcNext /* 只有下条指令需要跨行时，才不能发射 */
        canIn := rvcFinish || rvcForceLoadNext
        pcOut := io.in.bits.pc
        /* 正常情况pnpcOut是IFU的输出，当rvcFinish为0时，由当前指令计算出下条指令的PC（+2或者+4） */
        pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, io.in.bits.pc+2.U, io.in.bits.pc+4.U))
        when(io.out.fire() && rvcFinish){state := s_idle}
        when(io.out.fire() && rvcNext){
          state := s_extra
          pcOffsetR := pcOffset + Mux(isRVC, 2.U, 4.U)
        }
        /* 下条指令跨行，但不是跳转指令 */
        when(rvcSpecial && io.in.valid){
          state := s_waitnext
          specialPCR := pcOut
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
        /* 下条指令跨行，但是跳转指令 */
        when(rvcSpecialJump && io.in.valid){
          state := s_waitnext_thenj
          specialPCR := pcOut
          specialNPCR := io.in.bits.pnpc
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
      }
      /* 对地址不对齐的指令处理 */
      is(s_extra){//get 16 aligned inst, pc controled by this FSM
        canGo := rvcFinish || rvcNext
        canIn := rvcFinish || rvcForceLoadNext
        /* PC需要计算得出，不是IFU给出的PC */
        pcOut := Cat(io.in.bits.pc(VAddrBits-1,3), pcOffsetR(2,0)) 
        pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        /* 后面的和idle状态一样 */
        when(io.out.fire() && rvcFinish){state := s_idle}
        when(io.out.fire() && rvcNext){
          state := s_extra
          pcOffsetR := pcOffset + Mux(isRVC, 2.U, 4.U)
        }
        when(rvcSpecial && io.in.valid){
          state := s_waitnext
          specialPCR := pcOut
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
        when(rvcSpecialJump && io.in.valid){
          state := s_waitnext_thenj
          specialPCR := pcOut
          specialNPCR := io.in.bits.pnpc
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
      }
      /* 等待下一行指令数据，与跨行指令的另一半进行拼接 */
      is(s_waitnext){//require next 64bits, for this inst has size 32 and offset 6
        //ignore bp result, use pc+4 instead
        pcOut := specialPCR
        pnpcOut := specialPCR + 4.U
        // pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        canGo := io.in.valid
        canIn := false.B
        when(io.out.fire()){
          state := s_extra
          pcOffsetR := "b010".U
        }
      }
      is(s_waitnext_thenj){//require next 64bits, for this inst has size 32 and offset 6
        //use bp result
        pcOut := specialPCR
        pnpcOut := specialNPCR
        // pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        canGo := io.in.valid
        canIn := true.B
        when(io.out.fire()){
          state := s_idle
        }
      }
    }
  }.otherwise{
    state := s_idle
    canGo := DontCare
    canIn := DontCare
    pcOut := DontCare
    pnpcOut := DontCare
  }

  //output signals
  io.out.bits := DontCare
  io.out.bits.redirect.valid := false.B
  io.out.bits.pc := pcOut
  io.out.bits.pnpc := pnpcOut
  io.out.bits.instr := instr
  /* 为什么一个取反要用Mux？
    brIdx = !(pnpcOut == pcOut+(isRVC?2:4));
  */
  io.out.bits.brIdx := Mux((pnpcOut === pcOut+4.U && !isRVC) || (pnpcOut === pcOut+2.U && isRVC), false.B, true.B)

  io.out.valid := io.in.valid && canGo
  io.in.ready := (!io.in.valid || (io.out.fire() && canIn) || loadNextInstline)
  /* 异常相关，看不懂开摆 */
  io.out.bits.exceptionVec := io.in.bits.exceptionVec
  io.out.bits.exceptionVec(instrPageFault) := io.in.bits.exceptionVec(instrPageFault) || specialIPFR && (state === s_waitnext_thenj || state === s_waitnext)
  io.out.bits.crossPageIPFFix := io.in.bits.exceptionVec(instrPageFault) && (state === s_waitnext_thenj || state === s_waitnext) && !specialIPFR
}