package org.okarmus
package chapter13

import chapter11.Monad

import scala.io.StdIn.readLine

trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }

}

object IO extends Monad[IO]{
  override def unit[A](a: => A): IO[A] = new IO[A] {override def run: A = a}
  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def ref[A](value: A): IO[IORef[A]] = IO{ new IORef(value) }
  sealed class IORef[A](var value: A) {
    def set(a: => A): IO[A] = IO{ value = a; a }
    def get: IO[A] = IO{ value }
    def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
  }


  def PrintLine(msg: String): IO[Unit] = new IO[Unit] {
    override def run: Unit = println(msg)
  }

  def ReadLine: IO[String] = new IO[String] {
    override def run: String = readLine
  }
}
