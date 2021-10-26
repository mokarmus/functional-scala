package org.okarmus
package chapter5

import chapter5.Stream.{cons, constantUsingUnfold, empty, fibs, fibsUsingUnfold, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption(): Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  //Ex 5.1

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailrec: List[A] = {
    @tailrec
    def loop(toCheck: Stream[A], acc: List[A]): List[A] = toCheck match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, Nil: List[A]).reverse
  }

  //Ex 5.2
  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => empty
    case (_, Empty) => empty
    case (_, Cons(h, t)) => Cons(() => h(), () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (_, Cons(_, t)) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  //Ex 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((next, acc) => p(next) || acc)

  //Ex 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((next, acc) => p(next) && acc)

  //Ex 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((next, acc) => if (p(next)) cons(next, acc) else empty[A])

  // Ex 5.6
  def headOptionUsingFoldRight(): Option[A] = foldRight(None: Option[A])((next, _) => Some(next))

  //Ex 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((next, acc) => cons(f(next), acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((next, acc) => f(next).append(acc))

  def append[B >: A](bs: => Stream[B]): Stream[B] = foldRight(bs)((next, acc) => cons(next, acc))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((next, acc) => if (p(next)) cons(next, acc) else acc)

  def find(p: A => Boolean): Option[A] = filter(p).headOption() //It is efficient only due to stream nature and laziness

  //Ex 5.13
  def mapUsingUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeUsingUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (s, n) => (s, n) match {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, bs)) {
    case (as, bs) => (as, bs) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, bs)) {
    case (as, bs) => (as, bs) match {
      case (Empty, Empty) => None
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), bs))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (as, tb()))
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))

    }
  }

  //Ex. 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).find{ case (stream, prefix) => (stream, prefix) match {
    case (Some(s), Some(p)) if s != p => true
    case _ => false
  } }.isEmpty

  //Ex. 5.15
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s@Cons(_, t) => Some((s, t()))
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  //Ex 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???  //TODO implement it one day as it is very hard !! !! !! !!



}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  //Ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  //Ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //Ex 5.10
  def fibs(): Stream[Int] = {
    def loop(curr: Int, next: Int): Stream[Int] = cons(curr, loop(next, curr + next))

    loop(0, 1)
  }

  //Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, newS)) => cons(a, unfold(newS)(f))
  }

  //Ex 5.12
  def fibsUsingUnfold(): Stream[Int] = unfold((0, 1)) { case (curr, next) => Some(curr, (next, curr + next)) }

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def ones(): Stream[Int] = constantUsingUnfold(1)
}

object StreamTest extends App {

  val stream = Stream(1, 2, 3, 4)
  val list = List(1, 2, 3, 4)

  println(stream.toList == list)
  println(stream.toListTailrec == list)
  println(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
  println(Stream(1, 2, 3, 4, 5).drop(3).toList == List(4, 5))

  println(Stream(1, 2, 3, 4, 5).takeWhile(_ != 3).toList == List(1, 2))
  println(stream.forAll(_ < 5))
  println(!stream.forAll(_ % 2 == 0))
  println(Stream(1, 2, 3, 4, 5).takeWhileUsingFoldRight(_ != 3).toList == List(1, 2))
  println(stream.headOptionUsingFoldRight().contains(1))
  println(empty[Int].headOptionUsingFoldRight().isEmpty)

  println(stream.append(Stream(5, 6, 7)).toList == List(1, 2, 3, 4, 5, 6, 7))

  println(Stream("a", "bc", "cd").flatMap(x => Stream(x.toCharArray: _*)).toList == List('a', 'b', 'c', 'c', 'd'))

  println(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))

  val ones: Stream[Int] = Stream.cons(1, ones)

  println(unfold(1)(x => Some((x, x + 1))).take(3).toList == List(1, 2, 3))

  println(fibsUsingUnfold().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))

  println(constantUsingUnfold("x").take(3).toList == List("x", "x", "x"))

  println(Stream(1, 2, 3).mapUsingUnfold(_ + 10).toList == List(11, 12, 13))

  println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  println(!Stream(1, 2, 3).startsWith(Stream(2, 3)))

  println(Stream(1, 2, 3).tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))

}