package org.okarmus
package chapter13

import chapter11.Monad

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(s => f(run(s)))

  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(s => f(run(s)).run(s))

}

object ConsoleReader {

  implicit val monad: Monad[ConsoleReader] = new Monad[ConsoleReader] {
    override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)

    override def flatMap[A, B](fa: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] = fa flatMap f
  }
}
