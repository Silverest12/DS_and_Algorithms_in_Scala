package com.dataStructures.tree.bst

class NonEmptyTree[A](e: A, l: BST[A], r: BST[A])(implicit ev$1: A => Ordered[A]) extends BST[A] {
  def elem: A = e

  def right: BST[A] = r

  def left: BST[A] = l

  def setLeft[B >: A](newLeft: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] =
    new NonEmptyTree(e, newLeft, r)

  def setRight[B >: A](newRight: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] =
    new NonEmptyTree(e, l, newRight)

  def add[B >: A](newElem: B)(implicit ev$1: B => Ordered[B]): BST[B] = {
    if (newElem <= elem) setLeft(l add newElem)
    else setRight(r add newElem)
  }

  def concat[B >: A](that: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] = {
    if (that.nonEmpty)
      (this + that.elem).concat(that.right).concat(that.left)
    else this
  }

  def isEmpty: Boolean = false

  def nonEmpty: Boolean = true

  def head: A = elem

  def tail: (BST[A], BST[A]) = (l, r)

  def isValid: Boolean =
    if (l.elem > elem || r.elem < elem) false else (l isValid) && (r isValid)

  def contains[B >: A](newElem: B)(implicit ord: B => Ordered[B]): Boolean = {
    if (elem > newElem) l contains elem
    else if (elem < newElem) r contains elem
    else true
  }

  def forEach(func: A => Unit): Unit = {
    func(e)
    l.forEach(func)
    r.forEach(func)
  }

  def filterAcc[B >: A](pred: B => Boolean)(acc: BST[B])(implicit ev$1: B => Ordered[B]): BST[B] =
    right.filterAcc(pred)(left.filterAcc(pred)(if (pred(elem)) acc + elem else acc))

  def flatten: List[A] = l.flatten ++ List(e) ++ r.flatten

  def flatMap[B](func: A => BST[B])(implicit ev$1: B => Ordered[B]): BST[B] =
    foldPreOrder(func(elem))((e, acc) => acc ::: func(e))

  def map[B](func: A => B)(implicit ev$1: B => Ordered[B]): BST[B] =
    foldPreOrder[BST[B]](BST(func(elem)))((e, acc) => acc + func(e))

  def foldInOrder[B](x: B)(func: (A, B) => B): B =
    r.foldInOrder(func(e, l.foldInOrder(x)(func)))(func)

  def foldPreOrder[B](x: B)(func: (A, B) => B): B =
    r.foldPreOrder(l.foldPreOrder(func(elem, x))(func))(func)

  def foldPostOrder[B](x: B)(func: (A, B) => B): B =
    func(elem, r.foldPostOrder(l.foldPostOrder(x)(func))(func))

  def takeWhile (pred: A => Boolean) : BST[A] =
    if (pred(e)) BST (e) ::: l.takeWhile(pred) ::: r.takeWhile(pred)
    else EmptyTree

  def dropWhile(pred: A => Boolean) : BST[A] =
    if (pred(e)) l.dropWhile(pred) ::: r.dropWhile(pred)
    else this

  override def toString: String = elem + "[l=" + l + ", r=" + r + "]"
}

