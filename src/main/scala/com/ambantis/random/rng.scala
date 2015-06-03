package com.ambantis.random

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
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
