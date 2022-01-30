package org.okarmus
package chapter13

import chapter7.ParOps.Par
import chapter11.Monad
import chapter13.Translate.~>

sealed trait Free[F[_], A] {
  //Ex 13.1
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

}

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  //Ex. 13.1
  def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  //Ex. 13.2
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(f) => f()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(a0, g) => runTrampoline(a0 flatMap (a => g(a).flatMap(f)))
    }
  }

  //Ex. 13.3
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }


  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
}