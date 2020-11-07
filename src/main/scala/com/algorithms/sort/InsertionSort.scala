package com.algorithms.sort

object InsertionSort extends Sort {
  def sort[T](xs: List[T])(comparator: (T, T) => Boolean): List[T] = {
    def insert(xs: List[T], x: T) = {
      val (y, ys) = xs.span(comparator(_, x))
      y ::: (x :: ys)
    }

    xs.foldLeft(List[T]()) { (acc, a) => insert(acc, a) }
  }
}
