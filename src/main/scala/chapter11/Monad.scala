package org.okarmus
package chapter11

import chapter8.Gen
import chapter6.State
import chapter12.Applicative

trait Monad[F[_]] extends Applicative[F] {
  self =>

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  //Ex 11.3
  override def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A])) { (a, acc) => map2(a, acc)(_ :: _) }

  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  //Ex. 11.4
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  override def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  //Ex. 11.5
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms
      .foldRight(unit(List.empty[A])) {
        (a, acc) => flatMap(f(a))(b => if (!b) acc else map2(unit(a), acc)(_ :: _))
      }

  //Ex. 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))

  //Ex. 11.8
  def flatMapUsingCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  //Ex. 11.12
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  //Ex.11.13
  def flatMapUsingJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def composeUsingJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
    join(map(f(a))(b => g(b)))

  //Ex. 12.11
  /*def compose[G[_]](G: Monad[G]) = new Monad[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = self.flatMap(a => G.flatMap(b => ))
  }*/
}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
  }

  //Ex. 1.11 - implementations for other monads are pretty the same
  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  //Ex. 11.2
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad: Monad[StateS] = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State(s => (a, s))

      override def flatMap[A, B](fa: StateS[A])(f: A => StateS[B]): StateS[B] = fa.flatMap(f)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
  }
}
