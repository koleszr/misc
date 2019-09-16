package kz.simulation

import java.util.concurrent.TimeUnit

import kz.errors
import kz.errors.MiscError.IOError
import kz.implicits._
import zio.clock._
import zio.clock.Clock
import zio.console._
import zio.random
import zio.random.Random
import zio.{App, Schedule, ZIO}

object ValueOfPiApp extends App {
  override def run(args: List[String]): ZIO[ValueOfPiApp.Environment, Nothing, Int] =
    (for {
      _          <- putStr("Enter the number of parallel executions: ")
      n          <- getStrLn.mapError(IOError).flatMap(_.toIntZio)

      _          <- putStr("Enter the overall number of iterations: ")
      iterations <- getStrLn.mapError(IOError).flatMap(_.toIntZio)

      dtAndPi    <- timed(estimatePi(n, iterations))
      (dt, pi)   =  dtAndPi
      _          <- putStrLn(s"The estimated value of PI is $pi")
      _          <- putStrLn(s"The calculation of PI took $dt ms")
    } yield ()).fold(_ => 1, _ => 0)

  def timed[R, E, A](program: ZIO[R, E, A]): ZIO[R with Clock, E, (Long, A)] =
    for {
      start  <- currentTime(TimeUnit.MILLISECONDS)
      result <- program
      stop   <- currentTime(TimeUnit.MILLISECONDS)
    } yield (stop - start, result)

  val randomPairGenerator: ZIO[Random, Nothing, (Double, Double)] =
    for {
      x <- random.nextDouble
      y <- random.nextDouble
    } yield (x, y)

  val distanceLessThanOne: ZIO[Random, Nothing, Boolean] =
    for {
      pair     <- randomPairGenerator
      (x, y)   =  pair
      distance =  Math.sqrt(x * x + y * y)
    } yield distance <= 1

  def collectPolicy(n: Int): Schedule[Boolean, List[Boolean]] = Schedule.recurs(n) *> Schedule.collectAll

  def distanceLessThanOneChunk(n: Int): ZIO[Random with Clock, Nothing, Int] =
    for {
      count <- distanceLessThanOne repeat collectPolicy(n - 1).map(_.foldLeft(0) {
        case (acc, true)  => acc + 1
        case (acc, false) => acc
      })
    } yield count

  def estimatePi(n: Int, iterations: Int): ZIO[Random with Clock, errors.MiscError, Double] =
    ZIO.collectAllParN(n)(partitionIterations(n, iterations).map(i => distanceLessThanOneChunk(i)))
      .map(inside => inside.sum * 4 / iterations.toDouble)

  def partitionIterations(n: Int, iterations: Int): List[Int] = {
    val remainder = iterations % n
    val (_, result) = List.fill(n)(iterations / n).foldLeft((remainder, List.empty[Int])) {
      case ((r, acc), x) if r <= 0 => (r, x :: acc)
      case ((r, acc), x)           => (r - 1, (x + 1) :: acc)
    }
    result
  }
}
