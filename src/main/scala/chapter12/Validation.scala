package org.okarmus
package chapter12

import java.util.Date

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {

  //Ex. 12.6
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(e, es), Success(_)) => Failure(e, es)
      case (Success(_), Failure(e, es)) => Failure(e, es)
      case (Failure(e1, es1), Failure(e2, es2)) => Failure(e1, es1 ++ (e2 +: es2))
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}


case class WebForm(name: String, birthDate: Date, phoneNumber: String)

object WebForm {

  def validName(name: String): Validation[String, String] =
    if (name.isEmpty) Failure("Name cannot be empty") else Success(name)

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")


  def validWebForm(name: String, birthDate: String, phone: String): Validation[String, WebForm] =
    Validation.validationApplicative.map3(
      validName(name),
      validBirthdate(birthDate),
      validPhone(phone)
    )(WebForm(_, _, _))

}