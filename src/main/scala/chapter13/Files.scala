package org.okarmus
package chapter13

trait Files[A]

case class OpenRead(file: String) extends Files[HandleR]
case class OpenWrite(file: String) extends Files[HandleW]
case class ReadLine(h: HandleR) extends Files[Option[String]]
case class WriteLine(h: HandleW, line: String) extends Files[Unit]


trait HandleR
trait HandleW

object Files {

  def loop(f: HandleR, c: HandleW): Free[Files, Unit] = for {
    line <- Suspend { ReadLine(f) }
    _ <- line match {
      case None => IO.unit(())
      case Some(s) => Suspend {
        WriteLine(FahrenheitToCelsius.fahrenheitToCelsius(s.toDouble))
      } flatMap (_ => loop(f, c))
    }
  } yield ()

  def convertFiles: Free[Files, Unit] = for {
    f <- Suspend(OpenRead("fahrenheit.txt"))
    c <- Suspend(OpenWrite("celsius.txt"))
    _ <- loop(f,c)
  } yield ()

}