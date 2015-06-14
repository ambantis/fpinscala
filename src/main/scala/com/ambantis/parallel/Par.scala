package com.ambantis.parallel

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}
import scala.util.{Failure, Success, Try}

trait Par[+A] {
  def unit(a: A): Par[A]
  def map2[B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
  def fork(a: => Par[A]): Par[A]
  def lazyUnit(a: => A): Par[A] = fork(unit(a))
  def run(a: Par[A]): A
}


object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /**
   * Exercise 7.3. Fix the implementation of map2 so that it respects the
   * contract of timeouts on `Future`
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(implicit timeout: Long, unit: TimeUnit): Par[C] =
    (es: ExecutorService) => {
      val start = System.currentTimeMillis()
      val af = a(es)
      Try(af.get(timeout, unit)) map { a =>
      val timeout2 = unit.toMillis(start - timeout)
      UnitFuture(f(a, b(es).get(timeout2, unit)))
      } get
    }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A] {
      def call = a(es).get
    })
}

