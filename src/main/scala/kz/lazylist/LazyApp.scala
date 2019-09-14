package kz.lazylist

import LazyAlgorithms._
import zio.{App, ZIO}
import zio.console._

object LazyApp extends App {

  import kz.errors._
  import kz.errors.MiscError._
  import kz.implicits._

  override def run(args: List[String]): ZIO[Console, Nothing, Int] =
    (for {
      _ <- factorialApp
      _ <- sqrtApp
      _ <- derivativeOfXSquaredApp
    } yield ()).fold(_ => 1, _ => 0)

  val factorialApp: ZIO[Console, MiscError, Unit] =
    for {
      _            <- putStr("Enter an integer to calculate its factorial: ")
      factorialStr <- getStrLn.mapError(IOError)
      n            <- factorialStr.toIntZio
      _            <- putStrLn(s"$n! = ${fact(n)}")
    } yield ()

  val sqrtApp: ZIO[Console, MiscError, Unit] =
    for {
      _            <- putStr("Enter a double to calculate its square root: ")
      sqrtStr      <- getStrLn.mapError(IOError)
      d            <- sqrtStr.toDoubleZio
      _            <- putStr("Enter a double for tolerance between two approximations: ")
      toleranceStr <- getStrLn.mapError(IOError)
      tolerance    <- toleranceStr.toDoubleZio
      _            <- putStrLn(s"Square root of $d = ${sqrt(d, tolerance)}")
    } yield ()

  val derivativeOfXSquaredApp: ZIO[Console, MiscError, Unit] =
    for {
      _            <- putStr("Enter x to calculate the derivative of x ^ 2: ")
      xStr         <- getStrLn.mapError(IOError)
      x            <- xStr.toDoubleZio
      _            <- putStr("Enter a double for tolerance between two approximations: ")
      toleranceStr <- getStrLn.mapError(IOError)
      tolerance    <- toleranceStr.toDoubleZio
      _            <- putStrLn(s"Derivative of x ^ 2 at $x = ${derivative(x, tolerance)(d => d * d)}")
    } yield ()
}
