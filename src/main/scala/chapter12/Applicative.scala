package org.okarmus
package chapter12

import chapter11.Monad

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(())) { (a, _) => f(a) }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, acc) => map2(f(a), acc)(_ :: _) }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  //Ex. 12.1
  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  //Ex 12.7
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def map2[A, B, C](fga: (F[A], G[A]), fgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
      val (fa, ga) = fga
      val (fb, gb) = fgb

      (self.map2(fa, fb)(f), G.map2(ga, gb)(f))
    }

    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
  }

  //Ex. 12.9
  def compose[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))

    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
  }

  //Ex. 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), facc) => map2(fv, facc) { (v, acc) => acc.updated(k, v) } }

}

object Applicative {

  def streamApplicative: Applicative[LazyList] = new Applicative[LazyList] {
    override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
      (fa zip fb).map(f.tupled)

    override def unit[A](a: => A): LazyList[A] = LazyList(a)
  }

  def main(args: Array[String]): Unit = {
    val listOfStream = List(LazyList(1, 2, 3), LazyList(4, 5, 6))
    val sequenced = streamApplicative.sequence(listOfStream)
  }

  //Ex. 12.5
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Right(value) => f(value)
      case Left(ex) => Left(ex)
    }
  }
}
