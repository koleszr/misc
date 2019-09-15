package kz.fiber

import kz.computation.Computation
import kz.computation.ZIOConcurrentComputation.compute
import kz.computation.Computation.syntax._
import kz.state._
import zio.console._
import zio.{App, Ref, ZIO}

import scala.annotation.tailrec

object FiberApp extends App {
  override def run(args: List[String]): ZIO[FiberApp.Environment, Nothing, Int] =
    (for {
      ref         <- Ref.make(Map.empty[Computation[Double], Double])
      environment =  new Console.Live with State.Live[Map[Computation[Double], Double]] {
        override val stateRef: Ref[Map[Computation[Double], Double]] = ref
      }
      _           <- areaOfCirclesProgram.provide(environment)
      cache       <- ref.get
      _           <- putStrLn(s"Cache: ${cache.mkString("\n\t", "\n\t", "")}")
    } yield ()).fold(_ => 1, _ => 0)

  val areaOfCirclesProgram: ZIO[Console with State[Map[Computation[Double], Double]], Nothing, Unit] =
    for {
      _                <- putStrLn("Computing the area of circles")
      areaComputations =  doubles(0d, 1d, 0.001).map(r => compute(areaOfCircle(r)).map(a => (r, a)))
      fibers           <- ZIO.forkAll(areaComputations)
      results          <- fibers.join
      _                <- putStrLn(results.map { case (r, a) => s"Area of $r = $a"} mkString "\n")
    } yield ()

  def areaOfCircle(r: Double): Computation[Double] = r.n * r.n * Math.PI.n

  def doubles(start: Double, end: Double, step: Double): List[Double] = {
    @tailrec
    def go(current: Double, acc: List[Double]): List[Double] =
      if (current > end) acc else go(current + step, current :: acc)

    go(start, Nil).reverse
  }
}
