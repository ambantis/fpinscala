package com.ambantis.datastructures

import scala.annotation.tailrec

sealed trait List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def reverse: List[A]
}

case object Nil extends List[Nothing] {
  def head = throw new IllegalArgumentException("call to head of empty list")
  def tail = throw new IllegalArgumentException("call to tail of empty list")
  def isEmpty = true
  def reverse = this
}
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  val isEmpty = false
  def reverse: List[A] = {
    @tailrec
    def loop(acc: List[A], rem: List[A]): List[A] = rem match {
      case Nil => acc
      case Cons(first, rest) => loop(Cons(first, acc), rest)
    }
    loop(Nil, this)
  }
}

object List {
  def sum(ints: List[Int]): Int = {
    @tailrec
    def loop(acc: Int, rem: List[Int]): Int = rem match {
      case Nil => acc
      case Cons(x,xs) => loop(x + acc, xs)
    }
    loop(0, ints)
  }

  def product(ds: List [Double]): Double = {
    @tailrec
    def loop(acc: Double, rem: List[Double]): Double = rem match {
      case Nil => acc
      case Cons(x,xs) => loop(acc*x, xs)
    }
    loop(1.0, ds)
  }

  def apply[A](as: A*): List [A] =
    if (as.isEmpty) Nil
    else Cons (as.head, apply (as.tail:_*))

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => throw new IllegalArgumentException("tail of empty list")
    case Cons(x,xs) => xs
  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => throw new IllegalArgumentException("cannot set head of empty list")
    case Cons(x,xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) l
    else l match {
      case Nil => Nil
      case Cons(x,xs) => drop(xs, n-1)
    }
  }

  @tailrec
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(x,xs) if f(x) => dropWhile(xs, f)
    case _ => ls
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def append3[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1)(a2)((a,z) => Cons(a,z))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("cannot take init of empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x,init(xs))
  }

  @tailrec
  def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs)(f(z,x))(f)
  }

  def sum2(ls: List[Int]): Int = foldRight(ls)(0)(_ + _)

  def product2(ls: List[Int]): Int = foldRight(ls)(1)(_ * _)

  def length[A](ls: List[A]): Int = foldLeft(ls)(0)((z,_) => z + 1)

  def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs)(z)(f))
  }

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls)(List[A]())((z,a) => Cons(a,z))

  def append2[A](xs: List[A], ys: List[A]): List[A] = foldLeft(xs)(ys)((z,a) => Cons(a,z))

  def concat[A](lss: List[List[A]]): List[A] = lss match {
    case Nil => List[A]()
    case Cons(x,xs) => foldLeft(xs)(x)(append)
  }

  def map[A,B](ls: List[A])(f: A => B): List[B] =
    foldRight(ls)(List[B]())((a, z) => Cons(f(a), z))

  def map2[A,B](ls: List[A])(f: A => B): List[B] = {
    reverse(foldLeft(ls)(List[B]())((z,a) => Cons(f(a),z)))
  }

  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    foldLeft(ls)(List[A]()) { (z, a) =>
      if (f(a)) Cons(a,z) else z
    }
  }

  def flatMap[A,B](ls: List[A])(f: A => List[B]): List[B] =
    foldLeft(ls)(List[B]())((z,a) => append(z, f(a)))

  def filter2[A](ls: List[A])(f: A => Boolean): List[A] =
    flatMap(ls)((a: A) => if (f(a)) List(a) else Nil)

  def zipWith[A,B](as: List[A], bs: List[A])(f: (A,A) => B): List[B] = {
    @tailrec
    def loop(acc: List[B], r1: List[A], r2: List[A]): List[B] = (r1, r2) match {
      case (_, Nil) | (Nil,_)       => reverse(acc)
      case (Cons(x,xs), Cons(y,ys)) => loop(Cons(f(x,y),acc), xs, ys)
    }
    loop(List[B](), as, bs)
  }
}
