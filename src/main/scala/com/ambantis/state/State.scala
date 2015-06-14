package com.ambantis.state

case class State[S,+A](run: S => (A,S)) {
  import State.unit

  def get[S]: State[S,S] = State(s => s -> s)

  def set[S](s: S): State[S, Unit] = State(_ => () -> s)

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

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
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  require(candies >= 0, "a machine cannot have negative candies")
  require(coins >= 0, "a machine cannot have negative coins")
}

object Machine {
  def apply(candies: Int): Machine = Machine(true, candies, 0)

  val handle: Input => State[Machine, (Int, Int)] = (i: Input) => State( m =>
    (i, m) match {

      // a machine that's out of candy ignores all inputs
      case (_, Machine(_, 0, _)) => (0, m.coins) -> m

      // Inserting a coin to a locked machine will cause it to unlock if
      // there's any candy left
      case (Coin, Machine(true, _, _)) =>
        (m.candies, m.coins + 1) -> m.copy(locked = false, coins = m.coins + 1)

      // Turning the knob on an unlocked machine will cause it to dispense candy
      // and become locked
      case (Turn, Machine(false, candies, _)) =>
        (m.candies - 1, m.coins) -> m.copy(locked = true, candies = m.candies - 1)

      // Turning the knob on a locked machine or inserting a coin into an unlocked
      // machine does nothing
      case _ => (m.candies, m.coins) -> m
      }
    )
  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case Nil =>
      State(m => (m.candies, m.coins) -> m)
    case head :: tail =>
      State(m => (handle(head).run(m) /: tail)((acc, i) => handle(i).run(acc._2)))
  }

  val machine = Machine(10)

  val inputs = List(Coin, Turn, Coin, Turn, Coin, Coin, Turn, Coin, Turn, Coin, Turn)
  lazy val simulation: ((Int, Int), Machine) = // ((5,5), Machine(true,5,5))
    simulateMachine(inputs).run(machine)
}

