package kz.cats

import cats.data.State
import kz.cats.Computation.{Add, Divide, Multiply, Pure, Subtract}

object CachedComputation {

  type ComputationCacheA[A] = State[Map[Computation[A], A], A]

  def compute[F: Fractional](computation: Computation[F]): ComputationCacheA[F] =
    for {
      maybeResult <- State.get[Map[Computation[F], F]].map(_.get(computation))
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
        _           <- State.modify[Map[Computation[F], F]](cache => cache + (parent -> result))
      } yield result

    maybeResult match {
      case Some(result) => State.pure(result)
      case None =>
        computation match {
          case Pure(p)                          => State.pure(p)
          case add @ Add(left, right)           => helper(add, left, right)(num.plus)
          case subtract @ Subtract(left, right) => helper(subtract, left, right)(num.minus)
          case multiply @ Multiply(left, right) => helper(multiply, left, right)(num.times)
          case divide @ Divide(left, right)     => helper(divide, left, right)(num.div)
        }
    }
  }
}
