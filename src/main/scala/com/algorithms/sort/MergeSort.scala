package com.algorithms.sort

object MergeSort extends Sort {
  def sort[T](list: List[T])(comparator: (T, T) => Boolean): List[T] = {
    @scala.annotation.tailrec
    def merge(lList: List[T], rList: List[T], acc: List[T] = List()): List[T] = (lList, rList) match {
      case (Nil, _) => rList.reverse ::: acc
      case (_, Nil) => lList.reverse ::: acc
      case (x :: lRest, y :: rRest) =>
        if (comparator(x, y)) merge(lRest, rList, x :: acc)
        else merge(lList, rRest, y :: acc)
    }

    val n = list.length / 2
    if (n == 0) list
    else {
      val (rList, lList) = list splitAt n
      merge(sort(lList)(comparator), sort(rList)(comparator)).reverse
    }
  }
}
