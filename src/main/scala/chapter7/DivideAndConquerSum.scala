package org.okarmus
package chapter7

import java.util.concurrent.{ExecutorService, Executors}

object DivideAndConquerSum extends App {

  val es = Executors.newFixedThreadPool(10)

  private val parSum: ParOps.Par[Int] = ParOps.parallelSum(List(1, 2, 3, 4, 5))

  println(ParOps.run(parSum)(es))


  //TODO this is causing a deadlock !! !! !! !!
  val a = ParOps.lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(ParOps.equals(a, ParOps.delay(a))(S))

}

