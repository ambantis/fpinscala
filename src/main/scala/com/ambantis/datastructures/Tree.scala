package com.ambantis.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => math.max(max(l), max(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => math.max(1 + depth(l), 1 + depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A](t: Tree[A])(z: A)(f: (A,A) => A): A = t match {
    case Leaf(v) => f(v, z)
    case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }
}
