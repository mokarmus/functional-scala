package org.okarmus
package chapter7

import fpinscala.parallelism.Actor

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[A] {
  private[chapter7] def apply(a: A => Unit): Unit
}

object NonBlockingParOps {

  type Par[A] = ExecutorService => Future[A]

  def run[A](p: Par[A])(es: ExecutorService): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { res => ref.set(res); latch.countDown() }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    override private[chapter7] def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](pa: => Par[A]): Par[A] = es => new Future[A] {
    override def apply(cb: A => Unit): Unit = eval(pa(es)(cb))(es)
  }

  def eval[A](r: => Unit)(es: ExecutorService): Unit = es.submit(new Callable[Unit] {
    def call: Unit = r
  })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(cb(f(a, br.get)))(es)
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(cb(f(ar.get, b)))(es)
            else br = Some(b)
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))((n, acc) => map2(n, acc)(_ :: _) )


}
