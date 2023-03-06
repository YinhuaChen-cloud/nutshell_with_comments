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

package utils

import chisel3._
import chisel3.util._

object WordShift {
  def apply(data: UInt, wordIndex: UInt, step: Int) = (data << (wordIndex * step.U))
}

// map函数中，参数为 _ 是什么意思？ 回答：通常用来表示对输入集合中每个元素的操作
object MaskExpand {
  // 假如 m 为 0x0f, 则 m.asBools = Vec(4x false, 4x true), m.asBools.map(Fill(8, _)) = Vec(8个0， 8个0, ..., 8个1， 8个1)。 加上reverse = Vec(8个1， 8个1, ..., 8个0， 8个0)
  def apply(m: UInt) = Cat(m.asBools.map(Fill(8, _)).reverse) 
  // 注意，这里 reverse 的原因是: 在调用 asBools 之后， UInt 会转成一个数组， 数组下标 0~size 是从左到右，与 UInt 的下标从右到左正好相反。因此，最后需要调用 reverse 翻转整个 bit 串
}

object MaskData {
  def apply(oldData: UInt, newData: UInt, fullmask: UInt) = {
    (newData & fullmask) | (oldData & ~fullmask)
  }
}

object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    if (aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}
