package com.algorithms.sort

object QuickSort extends Sort {
  def sort[T](xs: List[T])(comparator: (T, T) => Boolean): List[T] = xs match {
    case Nil => xs
    case _ :: Nil => xs
    case x :: ys =>
      val (part1, part2) = ys.partition(comparator(_, x))
      sort(part1)(comparator) ::: (x :: sort(part2)(comparator))
  }
}
