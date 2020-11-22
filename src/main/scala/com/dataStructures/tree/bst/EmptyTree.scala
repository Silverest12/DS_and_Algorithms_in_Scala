package com.dataStructures.tree.bst

object EmptyTree extends BST[Nothing] {
  def elem: Nothing = None.get
  def left: BST[Nothing] = EmptyTree
  def right: BST[Nothing] = EmptyTree

  def setLeft[B >: Nothing](newLeft: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] = newLeft
  def setRight[B >: Nothing](newRight: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] = newRight

  def add[B >: Nothing](elem : B)(implicit ev$1: B => Ordered[B]) = new NonEmptyTree(elem, EmptyTree, EmptyTree)

  def concat[B >: Nothing](that: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] = that

  def contains[B >: Nothing](elem: B)(implicit ev$1: B => Ordered[B]): Boolean = false

  def isEmpty: Boolean = true
  def nonEmpty: Boolean = false

  def head: Nothing = throw new NoSuchElementException
  def tail: (BST[Nothing], BST[Nothing]) = throw new NoSuchElementException

  def isValid: Boolean = true

  def forEach(func: Nothing => Unit): Unit = None

  def filterAcc [B >: Nothing](pred: B => Boolean)(acc: BST[B])(implicit ev$1: B => Ordered[B]) : BST[B] = acc

  def flatten : List[Nothing] = List()

  def flatMap[B](func: Nothing => BST[B])(implicit ev$1: B => Ordered[B]): BST[B] = EmptyTree
  def map[B](func: Nothing => B)(implicit ev$1: B => Ordered[B]): BST[B] = EmptyTree

  def foldInOrder [B] (x: B) (func: (Nothing, B) => B): B = x
  def foldPreOrder [B] (x: B) (func: (Nothing, B) => B): B = x
  def foldPostOrder [B] (x: B) (func: (Nothing, B) => B): B = x

  def takeWhile (pred: Nothing => Boolean) : BST[Nothing] = EmptyTree
  def dropWhile (pred: Nothing => Boolean) : BST[Nothing] = EmptyTree

  override def toString = "[]"
}
