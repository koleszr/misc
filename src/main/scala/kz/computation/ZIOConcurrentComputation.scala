package kz.computation

import kz.state

object ZIOConcurrentComputation extends ZIOComputer {
  override protected def binaryCompute[F: Fractional](parent: Computation[F], left: Computation[F], right: Computation[F])
                                                     (f: (F, F) => F): ComputationCacheA[F] =
    for {
      leftFiber   <- compute(left).fork
      rightFiber  <- compute(right).fork
      leftResult  <- leftFiber.join
      rightResult <- rightFiber.join
      result      =  f(leftResult, rightResult)
      _           <- state.modify[Map[Computation[F], F]](cache => cache + (parent -> result))
    } yield result
}
