package org.okarmus
package chapter7

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

class Par[A] {

}

object ParOps {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val futureA = pa(es)
    val futureB = pb(es)
    UnitFuture(f(futureA.get, futureB.get))
  }

  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  def lazyUnit[A](a: => A): Par[A] = delay(unit(a))

  def run[A](a: Par[A])(es: ExecutorService): A = a(es).get()

  //Ex 7.3 (A bit copy pasted solution...)
  def map2WithTimeouts[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    private val futureA = pa(es)
    private val futureB = pb(es)

    @volatile private var cache: Option[C] = None

    override def isDone: Boolean = cache.isDefined

    override def get(): C = get(Long.MaxValue, TimeUnit.SECONDS)

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      futureA.cancel(mayInterruptIfRunning) ||
        futureB.cancel(mayInterruptIfRunning)
    }

    override def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

    override def get(timeout: Long, unit: TimeUnit): C = {
      val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
      val started = System.nanoTime()
      val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
      val elapsedTime = System.nanoTime() - started
      val b = futureB.get(timeoutNanos - elapsedTime, TimeUnit.NANOSECONDS)
      val c = f(a, b)
      cache = Some(c)
      c
    }
  }

  //Ex. 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(list: Par[List[Int]]): Par[List[Int]] = map(list)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = delay {
    sequence(ps.map(asyncF(f)))
  }

  //Ex. 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((next, list) => map2(next, list)(_ :: _))

  //Ex. 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = es =>
    UnitFuture(as.filter(a => run(asyncF(f)(a))(es)))

  def parserFilterOther[A](as: List[A])(f: A => Boolean): Par[List[A]] = delay {
    val pars: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def parallelSum(ints: List[Int]): Par[Int] = delay {
    if (ints.size <= 1) unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      map2(parallelSum(l), parallelSum(r))(_ + _)
    }
  }

  def divideAnConquerMax(ints: List[Int])(z: Int, f: (Int, Int) => Int): Par[Int] = delay {
    if (ints.size <= 1) unit(ints.headOption.getOrElse(z))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      map2(divideAnConquerMax(l)(z, f), divideAnConquerMax(r)(z, f))((lResult, rResult) => math.max(lResult, rResult))
    }
  }

  val wordCount: String => Int = _.split(" ").length

  def wordCountInParagraphs(paragraphs: List[String]): Par[Int] = delay {
    if (paragraphs.size <= 1) unit(paragraphs.headOption.map(wordCount).getOrElse(0))
    else {
      val (l, r) = paragraphs.splitAt(paragraphs.size / 2)
      map2(wordCountInParagraphs(l), wordCountInParagraphs(r))(_ + _)
    }
  }

  def divideAndConquer[A, B](as: List[A])(z: B, f: A => B, acc: (B, B) => B): Par[B] = {

    def inner(as: List[A]): Par[B] = {
      if (as.length <= 1) lazyUnit(as.headOption.map(f).getOrElse(z))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(inner(l), inner(r))(acc)
      }
    }

    inner(as)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = delay { //for is useful to compute everythin on a separate thread in a lazy way
    val parAB: Par[(A, B)] = map2(pa, pb)((a, b) => (a, b))
    map2(parAB, pc) { case ((a, b), c) => f(a, b, c) }
  }

  def equals[A](p1: Par[A], p2: Par[A])(es: ExecutorService): Boolean = p1(es).get() == p2(es).get()

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => {
    val bool = run(cond)(es)
    if (bool) t(es) else f(es)
  }

  //ex 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val int = run(n)(es)
    choices(int)(es)
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(bool => if (bool) 0 else 1))(List(t, f))

  //Ex. 7.12
  def choiceMap[K, V](pk: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val key = run(pk)(es)
    choices(key)(es)
  }

  //Ex. 7.13
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
    val a = run(pa)(es)
    f(a)(es)
  }

  def choiceNUsingChoices[A](n: Par[Int])(list: List[Par[A]]): Par[A] =
    flatMap(n) { i => list(i) }

  def choiceViaChoices[A](conf: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(conf) { b => if (b) t else f }

  //Ex. 7.14
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val innerPar = run(a)(es)
    innerPar(es)
  }

  def flatMapUsingJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

  def joinUsingFlatMap[A](pa: Par[Par[A]]): Par[A] =
    flatMap(pa)(x => x)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }


}

