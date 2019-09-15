package kz.computation

import kz.state

object ZIOCachedComputation extends ZIOComputer {
  override protected def binaryCompute[F: Fractional](parent: Computation[F], left: Computation[F], right: Computation[F])
                                                     (f: (F, F) => F): ComputationCacheA[F] =
    for {
      leftResult  <- compute(left)
      rightResult <- compute(right)
      result      =  f(leftResult, rightResult)
      _           <- state.modify[Map[Computation[F], F]](cache => cache + (parent -> result))
    } yield result
}
