package com.dataStructures.tree

class EmptyTree extends Tree[Nothing] {
  def value: Nothing = None.get
  def left = new EmptyTree
  def right = new EmptyTree

  def add[B >:Nothing](elem : B)(implicit ev$1: B => Ordered[B]): Tree[B] = new BinaryTree(elem, new EmptyTree, new EmptyTree)

  def contains[B >: Nothing](elem: B)(implicit ev$1: B => Ordered[B]): Boolean = false

  override def toString = "[]"
}
