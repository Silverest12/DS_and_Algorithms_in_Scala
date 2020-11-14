package com.dataStructures.tree

object EmptyTree extends Tree[Nothing] {
  def elem: Nothing = None.get
  def left: Tree[Nothing] = EmptyTree
  def right: Tree[Nothing] = EmptyTree

  def setLeft[B >: Nothing](newLeft: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] = newLeft
  def setRight[B >: Nothing](newRight: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] = newRight

  def add[B >: Nothing](elem : B)(implicit ev$1: B => Ordered[B]) = new BinaryTree(elem, EmptyTree, EmptyTree)

  def concat[B >: Nothing](that: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] = that

  def contains[B >: Nothing](elem: B)(implicit ev$1: B => Ordered[B]): Boolean = false

  def isEmpty: Boolean = true
  def nonEmpty: Boolean = false

  def head: Nothing = throw new NoSuchElementException
  def tail: (Tree[Nothing], Tree[Nothing]) = throw new NoSuchElementException

  def isValid: Boolean = true

  def forEach(func: Nothing => Unit): Unit = None

  def filterAcc [B >: Nothing](pred: B => Boolean)(acc: Tree[B])(implicit ev$1: B => Ordered[B]) : Tree[B] = acc

  def flatten : List[Nothing] = List()

  def flatMap[B](func: Nothing => Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] = EmptyTree
  def map[B](func: Nothing => B)(implicit ev$1: B => Ordered[B]): Tree[B] = EmptyTree

  def foldInOrder [B] (x: B) (func: (Nothing, B) => B): B = x
  def foldPreOrder [B] (x: B) (func: (Nothing, B) => B): B = x
  def foldPostOrder [B] (x: B) (func: (Nothing, B) => B): B = x

  override def toString = "[]"
}
