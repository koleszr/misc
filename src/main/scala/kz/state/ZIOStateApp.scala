package kz.state

import zio.App
import zio.console._
import zio.Ref
import zio.ZIO

import kz.cats.Computation
import kz.cats.Computation._
import kz.cats.Computation.syntax._

object ZIOStateApp extends App {

  type ComputationCacheA[F] = ZIO[State[Map[Computation[F], F]], Nothing, F]

  final def run(args: List[String]) =
    computationLogic.fold(_ => 1, _ => 0)

  val computationLogic =
    for {
      stateRef    <- Ref.make(Map.empty[Computation[Double], Double])
      computation = 2d.n + ((2d.n + 4d.n) * (2d.n + 4d.n))
      result      <- compute[Double](computation).provide(state.apply(stateRef))
      cache       <- stateRef.get
      _           <- putStrLn(s"Result: $result")
      _           <- putStrLn("Cache:")
      _           <- putStrLn(cache.mkString("\t", "\n\t", ""))
    } yield ()

  private def compute[F: Fractional](computation: Computation[F]): ZIO[State[Map[Computation[F], F]], Nothing, F] =
    for {
      maybeResult <- state.get[Map[Computation[F], F]].map(_.get(computation))
      result      <- computeWithState(maybeResult, computation)
    } yield result

  private def computeWithState[F](maybeResult: Option[F], computation: Computation[F])
                                 (implicit num: Fractional[F]): ComputationCacheA[F] = {
    def helper(parent: Computation[F], left: Computation[F], right: Computation[F])
              (f: (F, F) => F): ComputationCacheA[F] =
      for {
        leftResult  <- compute(left)
        rightResult <- compute(right)
        result      =  f(leftResult, rightResult)
        _           <- state.modify[Map[Computation[F], F]](cache => cache + (parent -> result))
      } yield result

    maybeResult match {
      case Some(result) => state.pure(result)
      case None =>
        computation match {
          case Pure(p)                          => state.pure(p)
          case add @ Add(left, right)           => helper(add, left, right)(num.plus)
          case subtract @ Subtract(left, right) => helper(subtract, left, right)(num.minus)
          case multiply @ Multiply(left, right) => helper(multiply, left, right)(num.times)
          case divide @ Divide(left, right)     => helper(divide, left, right)(num.div)
        }
    }
  }
}
