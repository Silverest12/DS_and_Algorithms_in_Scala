package com.algorithms.cipher

trait Cipher {

  def char2int(c: Char): Int = c.toInt - (if (c isLower) 'a'.toInt  else 'A'.toInt)

  def int2char(c: Int, firstId: Char): Char = (c + firstId.toInt).toChar

  def shift(shiftRate: Int, c: Char): Char =
    if (c isLower) int2char((char2int(c) + shiftRate + 26) % 26, 'a')
    else if (c isUpper) int2char((char2int(c) + shiftRate + 26) % 26, 'A')
    else c
}
