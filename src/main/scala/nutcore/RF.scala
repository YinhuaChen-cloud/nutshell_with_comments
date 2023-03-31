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

trait HasRegFileParameter {
  val NRReg = 32
}

class RegFile extends HasRegFileParameter with HasNutCoreParameter {
  val rf = Mem(NRReg, UInt(XLEN.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data(XLEN-1,0) }
} 

// 用来处理RAW(写后读)数据冒险
// 当IDU发现要写入某个寄存器时，把 busy(x) = 1
// 当WBU完成写入某个寄存器时，把 busy(x) = 0
// 在IDU阶段，若需要读出寄存器x，而此时 busy(x) = 1，说明发生了RAW
class ScoreBoard extends HasRegFileParameter {
  val busy = RegInit(0.U(NRReg.W))
  def isBusy(idx: UInt): Bool = busy(idx) 
  def mask(idx: UInt) = (1.U(NRReg.W) << idx)(NRReg-1, 0) // TODO: 这个不知道干嘛的
  def update(setMask: UInt, clearMask: UInt) = { // TODO: 似乎是用来更新 busy 数组的
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit when reg(i) is written
    // and issued at the same cycle.
    // Note that rf(0) is always free.
    busy := Cat(((busy & ~clearMask) | setMask)(NRReg-1, 1), 0.U(1.W))
  }
}
