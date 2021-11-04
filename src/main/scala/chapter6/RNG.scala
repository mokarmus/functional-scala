package org.okarmus
package chapter6


import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2), rng2)
  }

  //Ex 6.1
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, newRng) if i >= 0 => (i, newRng)
    case (_, newRng) => nonNegativeInt(newRng)
  }

  //Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), newRng)
  }

  //Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  //Ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight((List.empty[Int], rng)) { case (_, (list, rng)) => val (next, newRng) = rng.nextInt; (next :: list, newRng) }
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  //Ex 6.5
  def doubleWithMap(): Rand[Double] = map(_.nextInt)(_ / (Int.MaxValue + 1))

  //Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def randIntDouble: Rand[(Int, Double)] = map2(int, double)((_, _))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  //Ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (next, acc) =>
      map2(next, acc) {
        _ :: _
      }
    }

  def intsUsingSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  //Ex 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }
  }

  //Ex 6.9
  def mapUsingFlatMap[A, B](a: Rand[A])(f: A => B): Rand[B] =
    flatMap(a) { a => unit(f(a)) }

  def map2UsingFlatMap[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a) { aValue => mapUsingFlatMap(b) { bValue => f(aValue, bValue) } }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }
}

object SimpleRNGTest extends App {
  val rng = new SimpleRNG(12)

  //  (1 to 10).toList.foldRight(SimpleRNG(12): RNG){ (_, rng) =>  val x = rng.nextInt; println(x._1); x._2 } // an ugly code just for testing

  println(RNG.nonNegativeInt(rng)._1)

  println(RNG.double(rng)._1)
  println(RNG.ints(12)(rng))

}