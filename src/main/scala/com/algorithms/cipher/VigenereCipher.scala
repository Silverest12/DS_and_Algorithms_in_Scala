package com.algorithms.cipher

import scala.annotation.tailrec

// Need to implement the cipher cracking algorithm
// to be honest I got to look for it
object VigenereCipher extends Cipher {
  private def rotate(shiftKeys: List[Int])(msg: String) : String = {
    @tailrec
    def customZip(shiftList: List[Int], msg: List[Char], acc: List[(Char, Int)] = List()): List[(Char, Int)] = msg match {
      case List() => acc
      case x :: xs =>
        if(x.isLetter)
          customZip(if (shiftList.tail.isEmpty) shiftKeys else shiftList.tail, xs, (x, shiftList.head) :: acc)
        else customZip(shiftList, xs, (x, 0) :: acc)
    }

    (for((char, key) <- customZip(shiftKeys, msg.toList)) yield shift(key, char)).reverse.mkString
  }
  def encode(shiftKeys: String)(msg: String): String = rotate(shiftKeys.toList.map(char2int))(msg)

  def decode(shiftKeys: String)(msg: String): String = rotate(shiftKeys.toList.map(-char2int(_)))(msg)
}
