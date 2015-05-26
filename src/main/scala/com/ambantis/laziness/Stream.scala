package com.ambantis.laziness

import scala.annotation.unchecked
import Stream.unfold

sealed trait Stream[+A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def toList: scala.collection.immutable.List[A]
  def take(n: Int): Stream[A] = unfold((n, this)) {
    case (_, Empty) => None
    case (n, _) if n < 1 => None
    case (n, Cons(h, t)) => Some((h(), (n-1, t())))
  }
  def drop(n: Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, _) if !p(h()) => None
    case Cons(h, t) => Some((h(), t()))
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B
  def exists(p: A => Boolean): Boolean
  def forAll(p: A => Boolean): Boolean
  def headOption: Option[A] = foldRight(Option.empty[A]) { (a, acc) =>
    lazy val result = a
    if (Option(result).nonEmpty) Some(result) else acc
  }
  def map[B](f: => A => B): Stream[B] = unfold(this) {
    case Empty => Option.empty[(B, Stream[A])]
    case Cons(h, t) => Some((f(h()), t()))
  }
  def filter(f: => A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, acc) =>
    if (f(a)) Stream.cons(a, acc) else acc
  }
  def append[AA >: A](other: Stream[AA]): Stream[AA] = foldRight(other) { (a, acc) =>
      Stream.cons(a, acc)
  }
  def flatMap[B](f: => A => B): Stream[B] = foldRight(Stream.empty[B]) { (a, acc) =>
    Stream.cons(f(a), acc)
  }
  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, that)) {
    case (Cons(h1, t1), s2 @ Empty) => Some(((Some(h1()), None), (t1(), s2)))
    case (s1 @ Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (s1, t2())))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case _ => None
  }
  def startsWith[AA >: A](that: Stream[AA]): Boolean = (this, that) match {
    case (Cons(_, _), Cons(_, _)) => zipAll(that).foldRight(true) {
      case ((Some(a1), Some(a2)), acc) => acc && a1 == a2
      case ((_, None), acc) => true
      case _ => false
    }
    case _ => false
  }
  def tails: Stream[Stream[A]] = unfold(Option(this)) {
    case None => None
    case Some(e @ Empty) => Some(e -> None)
    case Some(s @ Cons(_, t)) => Some(s -> Option(t()))
  }
  def hasSubsequence[AA >: A](that: Stream[AA]): Boolean =
    tails exists (_ startsWith(that))
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B]
}

case object Empty extends Stream[Nothing] {
  val isEmpty = true
  def toList = List.empty[Nothing]
  def drop(n: Int): Stream[Nothing] = this
  def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
  def exists(p: Nothing => Boolean): Boolean = false
  def forAll(p: Nothing => Boolean): Boolean = false
  def scanRight[B](z: => B)(f: (Nothing, => B) => B): Stream[B] = Stream(z)
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  val isEmpty = false
  def toList: List[A] = h() :: t().toList
  def drop(n: Int): Stream[A] = {
    if (n < 1) this
    else t().drop(n - 1)
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = f(h(), t().foldRight(z)(f))
  def exists(p: A => Boolean): Boolean = p(h()) || t().exists(p)
  def forAll(p: A => Boolean): Boolean = p(h()) && t().forAll(p)
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z)) {
    case (a, acc) =>
      (acc: @unchecked) match {
        case Cons(h, t) => Stream.cons[B](f(a, h()), acc)
      }
  }
}

case object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((a, s)) => Stream.cons[A](a, unfold(s)(f))
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from(n: Int): Stream[Int] = unfold(n) { x => Some(x+1, x+1) }

  def fibs: Stream[Int] = Stream.cons[Int](0, unfold((1, 2)) { case (a, b) =>
    Some(a, (b, a+b))
  })
}
