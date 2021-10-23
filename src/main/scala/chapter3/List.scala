package org.okarmus
package chapter3

import chapter3.List._

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def fill[A](n: Int, a: A): List[A] = n match {
    case 0 => Nil
    case _ => Cons(a, fill(n - 1, a))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ints: List[Double]): Double = ints match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def tailrecSum(ints: List[Int]): Int = {
    def loop(toCheck: List[Int] = ints, sum: Int = 0): Int = toCheck match {
      case Nil => sum
      case Cons(h, t) => loop(t, sum + h)
    }

    loop()
  }

  //Ex 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("List is empty")
    case Cons(_, h) => h
  }

  //Ex 3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil => sys.error("List is empty")
    case Cons(_, t) => Cons(newHead, t)
  }

  //Ex 3.4
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (_, 0) => as
    case (Nil, _) => sys.error("List to short")
    case (Cons(_, t), _) => drop(t, n - 1)
  }

  //Ex 3.5
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => as
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case Nil => as2
    case Cons(h, t) => Cons(h, append(t, as2))
  }

  //Ex 3.6
  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("Can not remove from an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  //Ex 3.6
  //Not possible, cause we are traversing the list to the bottom before evaluating

  //Ex 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0) { (_, sum) => 1 + sum }

  //Ex 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //Ex 3.11
  def sumFoldLeft(as: List[Int]): Int = foldLeft(as, 0: Int)(_ + _)

  def productFoldLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def lengthFoldLeft(as: List[Int]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  //Ex 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((as, a) => Cons(a, as))

  //Ex 3.13
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = //TODO TBH, I do not understand it fully !! !! !! !!
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  //Ex 3.14
  def appendUsingFoldRight[A](as1: List[A], as2: List[A]): List[A] = foldRight(as1, as2)(Cons(_, _))

  //Ex 3.15
  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)

  //Ex 3.16
  def increment(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((next, acc) => Cons(next + 1, acc))

  //Ex 3.17
  def doubleToString(ds: List[Double]): List[String] = foldRight(ds, Nil: List[String])((next, acc) => Cons(next.toString, acc))

  //Ex 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((next, acc) => Cons(f(next), acc))

  //Ex 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((next, acc) => if (f(next)) Cons(next, acc) else acc)

  //Ex 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((next, acc) => append(f(next), acc))

  //Ex 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  //Ex 3.22
  def summarize(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, summarize(t1, t2))
  }

  //Ex 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def tailZipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def loop(l1: List[A], l2: List[B], result: List[C]): List[C] = (l1, l2) match {
      case (_, Nil) => result
      case (Nil, _) => result
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2),result))
    }
    reverse(loop(l1, l2, Nil))
  }

  //Ex 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(leftSup: List[A], leftSub: List[A]): Boolean = (leftSup, leftSub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hSup, tSup), Cons(hSub, tSub)) => if (hSup == hSub) loop(tSup, tSub) else loop(leftSup, sub)
    }
    loop(sup, sub)
  }

}

object ListTest {
  def main(args: Array[String]): Unit = {
    println(tailrecSum(List(1, 2, 3)) == 6)
    println(List.fill(12, "a"))

    //Ex 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x == 3)
    println("After drop " + drop(List(1, 2, 3, 4), 2))
    println(init(List(1, 2, 3, 4)))

    println(foldRight(List(1, 2, 3), 0)(_ + _) == 6)

    //Ex 3.8
    println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))

    println(length(List(1, 2, 3, 4, 5)) == 5)
    println(lengthFoldLeft(List(1, 2, 3, 4, 5)) == 5)

    println(reverse(List(1, 2, 3)) == List(3, 2, 1))

    println(appendUsingFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))

    println(flatten(List(List(1, 2), List(3), List(4, 5))) == List(1, 2, 3, 4, 5))

    println(increment(List(1, 2, 3)) == List(2, 3, 4))


    println(List.map(List(1, 2, 3))(_ * 2) == List(2, 4, 6))

    println(List.filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))

    List("dadas".toList)

    println(List.flatMap(List("as", "sdc"))(s => List(s.toList: _*)) == List('a', 's', 's', 'd', 'c'))

    println(filterUsingFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))

    println(summarize(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))

    println(hasSubsequence(List(1, 2, 2, 3, 4, 5), List(2, 3, 4)))

  }
}