package com.ambantis.state

import com.ambantis.random._

case class State[S,+A](run: S => (A,S)) {
  import State.unit

  def map[B](f: A => B): State[S, B] = flatMap { a =>
    unit(f(a))
  }

  def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] = flatMap { a =>
    sb map { b => f(a,b) }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => a -> s)

  def sequence[S,A](ss: List[State[S,A]]): State[S, List[A]] = {
    def loop(s: S, acc: List[A], rem: List[State[S,A]]): (List[A], S) = rem match {
      case Nil => acc.reverse -> s
      case head :: tail =>
        val (a2, s2) = head.run(s)
        loop(s2, a2 :: acc, tail)
    }
    State(s =>
      loop(s, List.empty[A], ss)
    )
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}
