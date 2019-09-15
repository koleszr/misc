package kz.computation

import kz.computation.Computation.{Add, Divide, Multiply, Pure, Subtract}
import kz.state

trait ZIOComputer {
  def compute[F](computation: Computation[F])(implicit num: Fractional[F]): ComputationCacheA[F] =
    for {
      maybeResult <- state.get[Map[Computation[F], F]].map(_.get(computation))
      result      <- maybeResult match {
        case Some(result) =>
          state.pure[Map[Computation[F], F], F](result)
        case None =>
          computation match {
            case Pure(p)                          => state.pure[Map[Computation[F], F], F](p)
            case add      @ Add(left, right)      => binaryCompute(add, left, right)(num.plus)
            case subtract @ Subtract(left, right) => binaryCompute(subtract, left, right)(num.minus)
            case multiply @ Multiply(left, right) => binaryCompute(multiply, left, right)(num.times)
            case divide   @ Divide(left, right)   => binaryCompute(divide, left, right)(num.div)
          }
      }
    } yield result

  protected def binaryCompute[F: Fractional](parent: Computation[F], left: Computation[F], right: Computation[F])
                                            (f: (F, F) => F): ComputationCacheA[F]
}
