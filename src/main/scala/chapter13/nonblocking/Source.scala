package org.okarmus
package chapter13.nonblocking

trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
}
