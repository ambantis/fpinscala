package com.ambantis.state

import scala.annotation.tailrec

trait RNG {
  import RNG._

  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  val double: Rand[Double] = map(int)(_ / Int.MaxValue.toDouble)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a =>
      unit(f(a))
  }

  def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb) { b => f(a,b)
      }
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A])) { (b, a) =>
      map2(b,a)(_ :: _)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { a =>
    val mod = a % n
    if (a + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def rollNDie(n: Int): Rand[List[Int]] = sequence(List.fill(n)(rollDie))

}

object RNG {

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    (i1, i2) -> rng2
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i2, rng2) = rng.nextInt
    if (i2 > 1) (i2, rng2)
    else -1 * (i2 + 1) -> rng2
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = rng.nextInt
    i / Int.MaxValue.toDouble -> rng1
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    (i, d) -> rng3
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    (d, i) -> rng2
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    (d1, d2, d3) -> rng3
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(acc: List[Int], r: RNG): (List[Int], RNG) = {
      if (acc.size >= count) acc -> r
      else {
        val next = r.nextInt
        loop(next._1 :: acc, next._2)
      }
    }
    loop(List.empty[Int], rng)
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    n -> nextRNG
  }
}
