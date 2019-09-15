package kz.state

import zio.App
import zio.console._
import zio.Ref
import zio.ZIO

import kz.computation.Computation
import kz.computation.ZIOCachedComputation.compute
import kz.computation.Computation.syntax._
import kz.state

object StateApp extends App {

  final def run(args: List[String]): ZIO[Console, Nothing, Int] =
    computationLogic.fold(_ => 1, _ => 0)

  val computationLogic: ZIO[Console, Nothing, Unit] =
    for {
      stateRef    <- Ref.make(Map.empty[Computation[Double], Double])
      computation =  2d.n + ((2d.n + 4d.n) * (2d.n + 4d.n))
      result      <- compute[Double](computation).provide(state.apply(stateRef))
      cache       <- stateRef.get
      _           <- putStrLn(s"Result: $result")
      _           <- putStrLn(s"Cache: ${cache.mkString("\n\t", "\n\t", "")}")
    } yield ()
}
