package org.okarmus
package chapter13

import chapter11.Monad

import scala.annotation.tailrec

object TailRecTesting {

  class TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

  }

  object TailRec extends Monad[TailRec] {
    override def unit[A](a: => A): TailRec[A] = Return(a)

    override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa flatMap f

    def suspend[A](a: => A): TailRec[A] = Suspend(() => a)
  }

  object TailrecTesting {
    def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))

    val p = TailRec.forever(printLine("Still going..."))


    @tailrec
    def run[A](io: TailRec[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a)) flatMap f)
      }
    }


    val f: Int => TailRec[Int] = x => Return(x)
    val g = List.fill(100000)(f).foldLeft(f) {
      (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
    }
  }

  case class Return[A](a: A) extends TailRec[A]

  case class Suspend[A](resume: () => A) extends TailRec[A]

  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

}
object TailrecIoMain extends App {

  println("it works")
}