package com.algorithms.cipher

object CeasarCipher extends Cipher {
  // Letter frequency in the english language
  // used for cracking the cypher using the
  // statistical occurrence f each letter in
  // the english words
  val table: List[Double] = List(8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1)

  def encode(shiftRate: Int)(msg: String): String = (for (x <- msg.toList) yield shift(shiftRate, x)).mkString

  def decode(shiftKey: Int)(msg: String): String = encode(-shiftKey)(msg)

  // This part below is all used for the cipher cracking
  // it's mainly uses statistical analysis of the characters
  // occurrence works on most cases but not all of them
  def percent(n: Int, m: Int): Double = (n.toDouble / m.toDouble) * 100

  def count(xs: List[Char], find: Char): Int = (for(x <- xs; if x == find) yield x).length

  def freqs(xs: List[Char]): List[Double] = (for (x <- 'a' to 'z') yield percent(count(xs, x), xs.length)).toList

  def chiSqr(os: List[Double], es: List[Double]): Double =
    (for((o, e) <- os zip es; diff = o-e) yield diff * diff / e).sum

  def rotate[a](n: Int, xs: List[a]):  List[a] =
    xs.drop(n) ++ xs.take(n)

  def positions[a](c: a, xs: List[a]): List[Int] =
    for ((x, i) <- xs.zipWithIndex; if x == c) yield i

  def crack(xs: String): String = {
    val freqTable = freqs(xs.toLowerCase.toList)
    val chiTable = for(n <- 0 to 25) yield chiSqr(rotate(n, freqTable), table)
    val factor = positions(chiTable.min, chiTable.toList).head
    System.out.println(factor)
    decode(factor)(xs)
  }
}
