package org.okarmus
package org.okarmus.chapter9

import scala.language.implicitConversions
import chapter8.Gen
import chapter8.Prop

import scala.util.matching.Regex

trait Parser[A]


trait Parsers[ParseError, Parser[+_]] {
  self =>

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(x => x))(in)

    def succeededLaw(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(succeeded(s))(s) == Right(s))
  }

  def run[A](p: Parser[A])(input: String): Either[Parsers.ParserError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeeded[A](a: A): Parser[A] = string("").map(_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  //Ex 9.1
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map { case (a, b) => f(a, b) }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  //Ex. 9.3
  def defer[A](p: => Parser[A]): Parser[A] = p

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, defer(many(p)))(_ :: _) or succeeded(List.empty[A])
  }

  //Ex 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeeded(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  //Ex. 9.6
  def nonNegativeInt(): Parser[Int] =
    regex("[0-9]".r).flatMap(s => s.toIntOption match {
      case None => fail("expected an integer")
      case Some(integer) => succeeded(integer)
    })

  //Ex 9.7
  def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] = for {
    a <- pa
    b <- pb
  } yield (a, b)

  def map2UsingFlatMap[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] = for {
    a <- pa
    b <- pb
  } yield f(a, b)

  //Ex. 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeeded(f(a)))

  def nConsecutiveAs: Parser[Int] = for {
    int <- nonNegativeInt()
    _ <- listOfN(int, succeeded("a"))
  } yield int

  //Ex. 9.9
  def whitespace: Parser[String] = "\\s+".r

  def token[A](p: Parser[A]): Parser[A] =
    p <* whitespace

  def number: Parser[String] = "[0-9]".r

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def skipR[A](pa: Parser[A], pany: Parser[Any]): Parser[A] =
    map2(pa, slice(pany))((a, b) => a)

  def skipL[B](pany: Parser[Any], pa: Parser[B]): Parser[B] =
    map2(slice(pany), pa)((_, a) => a)

  def fail(s: String): Parser[Nothing]

  implicit def regex(r: Regex): Parser[String]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  //val zeroOrMoreAFollowedByOneOrMoreB = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def slice: Parser[String] = self.slice(p)


    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

  }
}

object Parsers {
  type ParserError = String
}

object ParsersTesting extends App {


}