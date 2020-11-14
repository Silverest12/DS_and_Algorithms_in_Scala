package com.dataStructures.tree

trait Tree[+A] {
  def elem: A
  def left: Tree[A]
  def right: Tree[A]

  def setLeft[B >: A](newLeft: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B]
  def setRight[B >: A](newRight: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B]

  def add [B >: A](elem: B)(implicit ev$1: B => Ordered[B]) : Tree[B]
  def + [B >: A](elem: B)(implicit ev$1: B => Ordered[B]) : Tree[B] = add[B](elem)(ev$1)

  def concat [B >: A](that: Tree[B])(implicit ev$1: B => Ordered[B]) : Tree[B]
  def ::: [B >: A](that: Tree[B])(implicit ev$1: B => Ordered[B]) : Tree[B] = concat[B](that)(ev$1)

  def forEach(func: A => Unit) : Unit

  def flatten : List[A]

  def filter[B >: A](pred: B => Boolean)(implicit ev$1: B => Ordered[B]) : Tree[B] = filterAcc(pred)(EmptyTree)

  def filterAcc[B >: A](pred: B => Boolean)(acc: Tree[B])(implicit ev$1: B => Ordered[B]) : Tree[B]

  def flatMap [B](func: A => Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B]
  def map [B](func: A => B)(implicit ev$1: B => Ordered[B]) : Tree[B]

  def foldInOrder [B] (x: B) (func: (A, B) => B) : B
  def foldPreOrder [B] (x: B) (func: (A, B) => B) : B
  def foldPostOrder [B]( x: B) (func: (A, B) => B) : B

  def contains[B >: A](elem: B)(implicit ev$1: B => Ordered[B]): Boolean

  def isEmpty: Boolean
  def nonEmpty: Boolean

  def head: A
  def tail: (Tree[A], Tree[A])

  def isValid: Boolean


}
