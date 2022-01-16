package org.okarmus
package chapter12

import chapter10.Monoid
import chapter10.Foldable
import chapter11.Monad
import chapter6.State

import scala.language.implicitConversions

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_], A, B](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(ga => ga)

  //Ex. 12.14
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    traverse[Traverse.Id, A, B](fa)(f)(Traverse.idApplicative)
  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    import Traverse.monoidApplicative
    traverse[({type f[x] = Traverse.Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))
  }

  override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- State.get[List[A]]
      _ <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  def zipWithIndexUsingMapAccum[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, i) => ((a, i), i + 1))._1

  def toListUsingMapAccum[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, i) => ((), a :: i))._2.reverse

  //Ex. 12.16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  //Ex. 12.17
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, acc) => ((), f(acc, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("Incompatible types")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  //Ex. 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)


  //Ex. 12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }

  //Ex. 12.20
  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      F.flatMap(fga)(ga => {
        F.map(T.traverse(ga)(f)(F))(G.join)
      })
  }
}

case class IdApplicative[A](value: A)

object Traverse {

  //Ex. 12.3
  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B])) { (a, acc) => G.map2(f(a), acc)(_ :: _) }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }


  type Id[A] = A
  val idApplicative: Applicative[Id] = new Applicative[Id] {
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def unit[A](a: => A): Id[A] = a
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](m: Monoid[M]): Applicative[({
    type f[x] = Const[M, x]
  })#f] = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = m.op(fa, fb)

    override def unit[A](a: => A): Const[M, A] = m.zero
  }
}