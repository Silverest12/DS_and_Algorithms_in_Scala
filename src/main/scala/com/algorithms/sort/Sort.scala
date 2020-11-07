package com.algorithms.sort

trait Sort {
  def sort[T](xs: List[T])(comparator: (T, T) => Boolean): List[T]
}
