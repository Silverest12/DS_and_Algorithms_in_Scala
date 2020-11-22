package com.dataStructures.tree.bst

import scala.annotation.tailrec

trait BST[+A] {
  def elem: A
  def left: BST[A]
  def right: BST[A]

  def setLeft[B >: A](newLeft: BST[B])(implicit ev$1: B => Ordered[B]): BST[B]
  def setRight[B >: A](newRight: BST[B])(implicit ev$1: B => Ordered[B]): BST[B]

  def add [B >: A](elem: B)(implicit ev$1: B => Ordered[B]) : BST[B]
  def + [B >: A](elem: B)(implicit ev$1: B => Ordered[B]) : BST[B] = add[B](elem)(ev$1)

  def concat [B >: A](that: BST[B])(implicit ev$1: B => Ordered[B]) : BST[B]
  def ::: [B >: A](that: BST[B])(implicit ev$1: B => Ordered[B]) : BST[B] = concat[B](that)(ev$1)

  def forEach(func: A => Unit) : Unit

  def flatten : List[A]

  def filter[B >: A](pred: B => Boolean)(implicit ev$1: B => Ordered[B]) : BST[B] = filterAcc(pred)(EmptyTree)

  def filterAcc[B >: A](pred: B => Boolean)(acc: BST[B])(implicit ev$1: B => Ordered[B]) : BST[B]

  def flatMap [B](func: A => BST[B])(implicit ev$1: B => Ordered[B]): BST[B]
  def map [B](func: A => B)(implicit ev$1: B => Ordered[B]) : BST[B]

  def foldInOrder [B] (x: B) (func: (A, B) => B) : B
  def foldPreOrder [B] (x: B) (func: (A, B) => B) : B
  def foldPostOrder [B] (x: B) (func: (A, B) => B) : B

  def takeWhile (pred: A => Boolean) : BST[A]
  def dropWhile (pred: A => Boolean) : BST[A]

  def contains[B >: A](elem: B)(implicit ev$1: B => Ordered[B]): Boolean

  def isEmpty: Boolean
  def nonEmpty: Boolean

  def head: A
  def tail: (BST[A], BST[A])

  def isValid: Boolean
}

object BST {
  def apply[A]()(implicit ev$1: A => Ordered[A]): BST[A] = EmptyTree
  def apply[A](elem: A, elemL: A*)(implicit ev$1: A => Ordered[A]): BST[A] = {
    @tailrec
    def recurseAcc(xs: List[A], acc: BST[A]): BST[A] = xs match {
      case List() => acc
      case x :: xs => recurseAcc(xs, acc add x)
    }
    recurseAcc(elemL.toList, EmptyTree + elem)
  }
}

