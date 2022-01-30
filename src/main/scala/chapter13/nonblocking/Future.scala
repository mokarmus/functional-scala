package org.okarmus
package chapter13.nonblocking

import java.util.concurrent.ExecutorService
import chapter13.Free
import chapter13.Suspend


trait Future[+A] {
  def apply(k: A => Unit): Unit
}

object Future{

  type Par[+A] = ExecutorService => Future[A]

}

object Par {
  import Future.Par
  def async[A](run: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    override def apply(k: A => Unit): Unit = run(k)
  }

  def nonBlockingRead(source: Source, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = async{
    (cb: Either[Throwable, Array[Byte]] => Unit) => source.readBytes(numBytes, cb)
  }

  def readPar(source: Source, numBytes: Int): Free[Par, Either[Throwable, Array[Byte]]] = Suspend(nonBlockingRead(source, numBytes))

}