package org.okarmus
package chapter13
import chapter13.IO
import chapter13.IO._
import chapter11.Monad

object AmazingFactorial extends App  {

  private val helpstring =
    """
      |The Amazing Factorial REPL, v2.0
      |q - quit
      |<number> - compute the factorial of the given number
      |<anything else> - crash spectacularly
      |""".stripMargin


  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM[Int](1 to n to LazyList){ i => acc.modify{ _ * i }.skip }
    result <- acc.get
  } yield result

  def amazingFactorialProgram(): IO[_] = sequence_(
    IO.PrintLine(helpstring),
    doWhile{ ReadLine }{line =>
      when(line != "q") { for {
        n <- factorial(line.toInt)
        _ <- PrintLine(s"factorial: $n")
      } yield ()}
    }
  )

  amazingFactorialProgram().run


}
