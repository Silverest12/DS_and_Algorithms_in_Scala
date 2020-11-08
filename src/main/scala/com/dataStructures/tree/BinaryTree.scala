package com.dataStructures.tree

// get the ordered ds to implement in here

class BinaryTree[A](elem: A, l: Tree[A], r: Tree[A])(implicit ev$1: A => Ordered[A]) extends Tree[A] {
  def value: A = elem
  def left: Tree[A] = l
  def right: Tree[A] = r

  def add[B >: A](elem : B)(implicit ev$1: B => Ordered[B]): Tree[B] = {
    // how to implement comparison operators
    if (elem <= value) left add elem
    else right add elem
  }

  def contains[B >: A](elem: B)(implicit ev$1: B => Ordered[B]): Boolean = {
    if (elem < value) left contains elem
    else if (elem > value) right contains elem
    else true
  }

  override def toString: String = elem + "[l=" + left + ", r=" + right + "]"
}
