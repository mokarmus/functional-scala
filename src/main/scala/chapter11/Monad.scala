package org.okarmus
package chapter11

import chapter8.Gen
import chapter6.State
import chapter12.Applicative

import scala.language.implicitConversions

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

  def sequence_[A](fs: F[A]*): F[Unit] = foreachM[F[A]]{fs.to(LazyList)}(skip)

  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  //Ex. 11.4
  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def replicateM_[A](n: Int, ma: F[A]): F[Unit] = foreachM(LazyList.fill(n)(ma))(skip)

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

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    flatMap(a)(_ => t)
  }

  def as[A, B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  def skip[A](a: F[A]): F[Unit] = as(a)(())

  def foldM[A, B](s: LazyList[A])(z: B)(f: (B, A) => F[B]): F[B] = s match {
    case h #:: t => flatMap(f(z, h))(r => foldM(t)(r)(f))
    case _ => unit(z)
  }

  def foldM_[A, B](s: LazyList[A])(z: B)(f: (B, A) => F[B]): F[Unit] = skip(foldM(s)(z)(f))
  def foreachM[A](s: LazyList[A])(f: A => F[Unit]): F[Unit] =
    foldM_(s)(())((u, a) => skip(f(a)))

  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] = if (b) as(fa)(true) else unit(false)


  implicit def toMonadic[A](a: F[A]): Monadic[F, A] = new Monadic[F,A] {
    override val F: Monad[F] = Monad.this
    override def get: F[A] = a
  }
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

trait Monadic[F[_], A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def **[B](b: F[B]) = F.map2(a,b)((_,_))
  def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
  def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
  def replicateM(n: Int) = F.replicateM(n, a)
  def replicateM_(n: Int) = F.replicateM_(n, a)
}
