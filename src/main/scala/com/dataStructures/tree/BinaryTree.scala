package com.dataStructures.tree

import scala.annotation.tailrec

class BinaryTree[A](e: A, l: Tree[A], r: Tree[A])(implicit ev$1: A => Ordered[A]) extends Tree[A] {
  def elem : A       = e
  def right: Tree[A] = r
  def left : Tree[A] = l

  def setLeft[B >: A](newLeft: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] =
    new BinaryTree(e, newLeft, r)

  def setRight[B >: A](newRight: Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] =
    new BinaryTree(e, l, newRight)

  def add [B >: A](newElem : B)(implicit ev$1: B => Ordered[B]): Tree[B] = {
    if (newElem <= elem) setLeft(l add newElem)
    else setRight(r add newElem)
  }

  def concat [B >: A](that : Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] = {
    if (that.nonEmpty)
      (this + that.elem).concat(that.right).concat(that.left)
    else this
  }

  def isEmpty: Boolean = false
  def nonEmpty: Boolean = true

  def head: A = elem
  def tail: (Tree[A], Tree[A]) = (l, r)

  def isValid: Boolean =
    if (l.elem > elem || r.elem < elem) false else (l isValid) && (r isValid)

  def contains[B >: A](newElem: B)(implicit ord: B => Ordered[B]): Boolean = {
    if (elem > newElem) l contains elem
    else if (elem < newElem) r contains elem
    else true
  }

  def forEach(func: A => Unit) : Unit = {
    func(e)
    l.forEach(func)
    r.forEach(func)
  }

  def filterAcc[B >: A](pred: B => Boolean)(acc: Tree[B])(implicit ev$1: B => Ordered[B])  : Tree[B] =
    right.filterAcc(pred)(left.filterAcc(pred)(if(pred(elem)) acc + elem else acc))

  def flatten : List[A] = l.flatten ++ List(e) ++ r.flatten

  def flatMap[B](func: A => Tree[B])(implicit ev$1: B => Ordered[B]): Tree[B] =
    foldPreOrder(func(elem))((e, acc) => acc ::: func(e))

  def map[B](func: A => B)(implicit ev$1: B => Ordered[B]): Tree[B] =
    foldPreOrder[Tree[B]](BinaryTree(func(elem)))((e, acc) => acc + func(e))

  def foldInOrder[B](x: B)(func: (A, B) => B): B =
    r.foldInOrder(func(e, l.foldInOrder(x)(func))) (func)

  def foldPreOrder[B](x: B)(func: (A, B) => B): B =
    r.foldPreOrder(l.foldPreOrder(func(elem, x))(func))(func)

  def foldPostOrder[B](x: B)(func: (A, B) => B): B =
    func(elem, r.foldPostOrder(l.foldPostOrder(x)(func))(func))

  override def toString: String = elem + "[l=" + l + ", r=" + r + "]"
}

object BinaryTree {
  def apply[A]()(implicit ev$1: A => Ordered[A]): Tree[A] = EmptyTree
  def apply[A](elem: A, elemL: A*)(implicit ev$1: A => Ordered[A]): Tree[A] = {
    @tailrec
    def recurseAcc(xs: List[A], acc: Tree[A]): Tree[A] = xs match {
      case List() => acc
      case x :: xs => recurseAcc(xs, acc add x)
    }
    recurseAcc(elemL.toList, EmptyTree + elem)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    def bst: Tree[Int] = BinaryTree(6,2,3,4,5)
    print(bst filter (_%2 == 0))
  }
}
