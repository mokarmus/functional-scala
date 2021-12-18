package org.okarmus
package chapter7

import java.util.concurrent.Executors

object ActorTesting extends App {

  val es = Executors.newFixedThreadPool(4)

/*  val echoer = Actor[String](es)(msg => println(s"Got message: $msg"))

  echoer ! "Hello"
  echoer ! "Hello2"*/


  val par = NonBlockingParOps.parMap(List.range(1, 100))(math.sqrt(_))

  val x = NonBlockingParOps.run(par)(es)

  println(s"The list is ${x}")

}
