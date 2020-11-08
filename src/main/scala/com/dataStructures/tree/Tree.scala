package com.dataStructures.tree

trait Tree[+A] {
  def value: A
  def left: Tree[A]
  def right: Tree[A]

  def add[B >: A](elem: B)(implicit ev$1: B => Ordered[B]): Tree[B]
  //def insert(elem: T): Tree[T]
  //def isEmpty: BooleAn
  //def nonEmpty: BooleAn
  //def heAd: A
  //def isValid: BooleAn
  def contains[B >: A](elem: B)(implicit ev$1: B => Ordered[B]): Boolean
}
