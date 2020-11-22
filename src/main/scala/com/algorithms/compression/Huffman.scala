package com.algorithms.compression

import scala.annotation.tailrec

object Huffman {
  sealed trait Tree[+A]
  case class Leaf[A] (value: A) extends Tree[A]
  case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

  type Bit = Int

  @tailrec
  def merge(xs: List[(Tree[Char],  Int)]): List[(Tree[Char], Int)] = {
    if (xs.length == 1) xs
    else {
      val l = xs.head
      val r = xs.tail.head
      val merged = (Branch(l._1, r._1), l._2 + r._2)
      merge ((merged :: xs.drop(2)).sortBy(_._2))
    }
  }

  def contains (tree: Tree[Char], char: Char): Boolean = tree match {
    case Leaf (c)      => if(c == char) true else false
    case Branch (l, r) => contains(l, char) || contains(r, char)
  }

  def freq (text: String): List[(Leaf[Char], Int)] = {
    text.toSeq.groupBy(c => c).view.mapValues(_.length).toList.map(x => (Leaf(x._1), x._2)).sortBy(_._2)
  }

  def encodeChar (tree: Tree[Char], char: Char): List[Bit] = {
    @tailrec
    def codeAcc(tree: Tree[Char], char: Char, code: List[Bit] = List()): List[Bit] = tree match {
      case Leaf(_) => code
      case Branch(l, r) =>
        if (contains(l, char)) codeAcc (l, char, 0 :: code)
        else codeAcc(r, char, 1 :: code)
    }
    codeAcc(tree, char)
  }

  // generates The bits mapping for the trees
  // so that it can be decoded later and also
  // speeds up the encoding process so it
  // doesn't need to process the tree each
  // time
  def generateBitMap (text: String): Map[Char, List[Bit]] = {
    val frequencies = freq(text)
    val codeTree = merge(frequencies).head._1
    
    frequencies.map(x => (x._1.value, encodeChar(codeTree, x._1.value))).toMap
  }

  def encode (codeTree: Map[Char, List[Bit]]) (text: String): List[List[Bit]] = {
    @tailrec
    def encodeAcc(textList: List[Char], acc: List[List[Bit]] = List()): List[List[Bit]] = textList match {
      case List() => acc
      case c::cs  => encodeAcc(cs, acc ::: List(codeTree(c)))
    }

    encodeAcc(text.toList)
  }

  def decode (codeTree: Map[List[Bit], Char])(bitList: List[List[Bit]]): String = {
    @tailrec
    def decodeAcc(bitList: List[List[Bit]], acc: String = ""): String = bitList match {
      case List()  => acc
      case b :: bs => decodeAcc(bs, acc + codeTree(b))
    }
    decodeAcc(bitList)
  }
}
