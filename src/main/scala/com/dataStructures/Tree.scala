package com.dataStructures

sealed trait Tree[+T] {
  def value: T
  def left: Tree[T]
  def right: Tree[T]

  //def insert(elem: T): Tree[T]
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def head: T
  def isValid: Boolean
}
