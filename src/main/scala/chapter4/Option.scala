package org.okarmus
package chapter4

import chapter4.Option.{map2, sequence, traverse}

trait Option[+A] {
  //Ex 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)



}


case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  //Ex 4.3
  def map2[A, B, C](opA: Option[A], opB: Option[B])(f: (A, B) => C): Option[C] = for {
    a <- opA
    b <- opB
  } yield f(a, b)

  //Ex 4.3
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Some(value) :: t => sequence(t).map(list => value :: list)
    case None :: _ => None
  }

  //Ex 4.4
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case a :: t => f(a).flatMap(b => traverse(t)(f).map(bt => b :: bt))
  }

}

object OptionTest {

  def variance(xs: Seq[Double]): Option[Double] = mean(xs)
    .flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] = xs.size match {
    case 0 => None
    case _ => Some(xs.sum / xs.size)
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case _: Exception => None }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 12.0

  def parseInsuranceRateQoute(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optNumberOfSpeedingTickets = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optNumberOfSpeedingTickets)(insuranceRateQuote)
  }

  def parseInts(l: List[String]): Option[List[Int]] = traverse(l)(i => Try(i.toInt))


  def main(args: Array[String]): Unit = {

    println(sequence(List(Some(1), None, Some(2))) == None)
    println(sequence(List(Some(1), Some(9), Some(2))) == Some(List(1, 9, 2)))


    println(parseInts(List("1", "12", "-3")) == Some(List(1, 12, -3)))
    println(parseInts(List("1", "a", "-3")) == None)


  }

}