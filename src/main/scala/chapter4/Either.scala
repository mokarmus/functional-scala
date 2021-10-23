package org.okarmus
package chapter4

trait Either[+E, +A] {

  //Ex 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(error) => Left(error)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this
    b <- eb
  } yield f(a, b)

}


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  //Ex 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, either) => f(a).map2(either)(_  :: _))

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(a => a)


}

object EitherTest {

  def mean(xs: Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean on empty list!")
    else Right(xs.sum / xs.size)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }


  def main(args: Array[String]): Unit = {

  }
}

//Using either to validate the data
case class Person(name: Name, age: Age) {

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int) = for {
    n <- mkName(name)
    a <- mkAge(age)
  } yield Person(n, a)

  //Ex 4.8
  def betterMap2[E, EE >: E, A, B, C](a: Either[E, A], b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (a, b) match {
    case (Left(e1), Left(e2)) => Left(List(e1, e2))
    case (Left(e1), _) => Left(List(e1))
    case (_, Left(e2)) => Left(List(e2))
    case (Right(value1), Right(value2)) => Right(f(value1, value2))
  }



}
sealed class Name(val value: String)
sealed class Age(val value: Int)

