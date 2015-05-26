package com.ambantis.datastructures

import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(get))

  def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  def getOrElse[B >: A](default: => B): B = get
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = {
    if (f(get)) this
    else None
   }
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = this
  def flatMap[B](f: Nothing => Option[B]): Option[B] = this
  def getOrElse[B](default: => B): B = default
  def orElse[B](ob: => Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = this
}

object Option {

  def variance(xs: Seq[Double]): Option[Double] = {
    def maybeMean =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    def v(x: Double)(implicit m: Double): Double = math.pow(x - m, 2)
    maybeMean map { implicit m =>
      xs.map(x => v(x)).sum / xs.length
    }
  }

  def map2[A,B,C](optA: Option[A], optB: Option[B])(f: (A,B) => C): Option[C] = {
    (optA, optB) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  }
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def loop(acc: List[A], rem: List[Option[A]]): Option[List[A]] = rem match {
      case Nil => Some(acc.reverse)
      case Cons(None, _) => None
      case Cons(Some(a), rest) => loop(Cons(a, acc), rest)
    }
    loop(Nil, as)
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def loop(acc: List[B], rem: List[A]): Option[List[B]] = rem match {
      case Nil => Some(acc.reverse)
      case Cons(h, t) =>
        f(h) match {
          case None => None
          case Some(b) =>
            loop(Cons(b, acc), t)
        }
    }
    loop(Nil, a)
  }

}
