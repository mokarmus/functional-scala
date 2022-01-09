package org.okarmus
package chapter10

import chapter8.{Gen, Prop}
import chapter7.ParOps._

object MonoidTesting {

  val stringMonoid = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = Nil

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  //Ex 10.1
  val intAddition = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  //Ex 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  //Ex 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = a => a

    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
  }

  //Ex 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    //Identity
    Gen.forAll(gen)(a => m.op(m.op(a, a), a) == m.op(a, m.op(a, a))) &&
      //Associativity
      Gen.forAll(for {
        a <- gen
        b <- gen
        c <- gen
      } yield (a, b, c)
      ) { case (a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //Ex. 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((bs, a) => m.op(bs, f(a)))

  //Ex. 10.6
  def foldLeftUsingFoldMap[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val bMonoid = new Monoid[B => B] {
      override def zero: B => B = b => b

      override def op(a1: B => B, a2: B => B): B => B = a2 compose a1
    }
    foldMap(as, bMonoid)(a => b => f(b, a))(z)
  }

  //Ex 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.toList match {
    case Nil => m.zero
    case h :: Nil => f(h)
    case _ =>
      val (l, r) = v.splitAt(v.length / 2)
      m.op(
        foldMapV(l, m)(f),
        foldMapV(r, m)(f)
      )
  }

  //Ex 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def zero: Par[A] = unit(m.zero)

    override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parMonoid = par(m)

    v.toList match {
      case Nil => parMonoid.zero
      case h :: Nil => unit(f(h))
      case _ =>
        val (l, r) = v.splitAt(v.length / 2)
        parMonoid.op(
          parFoldMap(l, m)(f),
          parFoldMap(r, m)(f)
        )
    }
  }

  //Ex. 10.9
  val isOrderedM: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
    override def zero: Option[(Int, Int, Boolean)] = None

    override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a1, a2) match {
      case (x, None) => x
      case (None, x) => x
      case (Some((x1, y1, b1)), Some((x2, y2, b2))) => Some((x1 min x2, y1 max y2, b1 && b2 && y1 <= x2))
    }
  }

  def ordered(v: IndexedSeq[Int]): Boolean = foldMapV(v, isOrderedM)(i => Some((i, i, true))).forall(_._3)


  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //Ex. 10.10
  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(l, w, r)) => Part(s1 + l, w, r)
      case (Part(l, w, r), Stub(s2)) => Part(l, w, r + s2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  //Ex. 10.11
  def unstub(s: String): Int = s.length min 1

  def wc(s: String): Int = {
    def count(c: Char): WC = {
      if (c.isWhitespace) {
        Part("", 0, "")
      } else {
        Stub(c.toString)
      }
    }

    foldMapV(s.toIndexedSeq, wcMonoid)(count) match {
      case Stub(chars) => unstub(chars)
      case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
    }
  }

  //Ex. 10.16
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a2._2, a2._2))
  }

  //Ex. 10.17
  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => b.zero

    override def op(f: A => B, g: A => B): A => B = a => b.op(f(a), g(a))
  }


  //Ex. 10.18
  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, key) => {
      acc.updated(key, v.op(a1.getOrElse(key, v.zero), a2.getOrElse(key, v.zero)))
    }
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV[A, Map[A, Int]](as, mapMergeMonoid(intAddition))((a: A) => Map(a -> 1))
  }


  def main(args: Array[String]): Unit = {
    //Prop.run(monoidLaws(intAddition, Gen.choose(0, 100)))

    println("I am compiling")
  }
}
