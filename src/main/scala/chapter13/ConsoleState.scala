package org.okarmus
package chapter13

import chapter11.Monad

case class Buffers(in: List[String], out: List[String])

case class ConsoleState[A](run: Buffers => (A, Buffers)) {

  def map[B](f: A => B): ConsoleState[B] = ConsoleState { s =>
    val (a, s1) = run(s)
    val b = f(a)
    (b, s1)
  }

  def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }
}

object ConsoleState {

  val monad: Monad[ConsoleState] = new Monad[ConsoleState] {
    override def unit[A](a: => A): ConsoleState[A] = ConsoleState { s => (a, s) }
    override def flatMap[A, B](fa: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] = fa flatMap f
  }

}
