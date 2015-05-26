package com.ambantis.datastructures

import scala.annotation.tailrec

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E,B]
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B): Either[E,Nothing] = this
  def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE,B] = this
  def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
}


case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: A => B): Either[Nothing,B] = Right(f(value))
  def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
}

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list")
    else Right (xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = {
    try Right(x/y)
    catch { case e: Throwable => Left(e) }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def loop(acc: List[A], rem: List[Either[E, A]]): Either[E, List[A]] = rem match {
      case Nil => Right(acc.reverse)
      case Cons(Left(e), rest) => Left(e)
      case Cons(Right(a), rest) => loop(Cons(a, acc), rest)
    }
    loop(Nil, es)
  }

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    @tailrec
    def loop(acc: List[B], rem: List[A]): Either[E, List[B]] = rem match {
      case Nil => Right(acc.reverse)
      case Cons(h, t) =>
        f(h) match {
          case Left(e) => Left(e)
          case Right(a) => loop(Cons(a, acc), t)
        }
    }
    loop(Nil, as)
  }


}
