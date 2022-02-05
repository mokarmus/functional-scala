package org.okarmus
package chapter13

import chapter7.ParOps.Par
import chapter7.ParOps

import scala.io.StdIn.readLine
import chapter13.Translate._
import chapter11.Monad

import scala.annotation.tailrec


sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A

  def toReader: ConsoleReader[A]
  def toState: ConsoleState[A]

}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToReader = new (Console ~> ConsoleReader) {
    override def apply[A](f: Console[A]): ConsoleReader[A] = f.toReader
  }

  val consoleToState = new (Console ~> ConsoleState) {
    override def apply[A](f: Console[A]): ConsoleState[A] = f.toState
  }

  val consoleToFunction0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = () => f(fa())()
  }
  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    Free.runFree[Console, Function0, A](a)(consoleToFunction0)

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = ParOps.lazyUnit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = ParOps.flatMap(fa)(f) //Probably this should be wrapped in a fork !! !! !! !!
  }
  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    Free.runFree[Console, Par, A](a)(consoleToPar)

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] = Free.runFree[Console, ConsoleReader, A](io)(consoleToReader)
  //Ex 13.4
  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      override def apply[A](f: F[A]): FreeG[A] = Suspend(fg(f))
    }
    Free.runFree(f)(t)(Free.freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A = Free.runTrampoline { translate(a)(consoleToFunction0) }

}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = ParOps.lazyUnit(run)
  override def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try { Some(readLine()) }
    catch { case _: Exception => None }

  override def toReader: ConsoleReader[Option[String]] = ConsoleReader{ in => Some(in) }

  def toState = ConsoleState { bufs =>
    bufs.in match {
      case List() => (None, bufs)
      case h :: t => (Some(h), bufs.copy(in = t))
    }
  }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = ParOps.lazyUnit(println(line))
  override def toThunk: () => Unit = () => println(line)

  override def toReader: ConsoleReader[Unit] = ConsoleReader { s => ()}
  override def toState: ConsoleState[Unit] =  ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) }
}

object ConsoleTesting extends App {
  import chapter13.Console._
  val f1: ConsoleIO[Option[String]] = for {
    _ <- printLn("I can only interact with console.")
    ln <- readLn
  } yield ln




  //Free.run(f1) TODO we are missing monad for console --> start from this tomorrow as we need to provide Translate monad

  println(Free.runFree(f1)(Console.consoleToFunction0).apply())

}