package kz.cats

import cats.data.State

sealed trait Computation[F]

object Computation {
  type ComputationCacheA[A] = State[Map[Computation[A], A], A]

  final case class Pure[F: Fractional](p: F) extends Computation[F]
  final case class Add[F: Fractional](left: Computation[F], right: Computation[F]) extends Computation[F]
  final case class Subtract[F: Fractional](left: Computation[F], right: Computation[F]) extends Computation[F]
  final case class Multiply[F: Fractional](left: Computation[F], right: Computation[F]) extends Computation[F]
  final case class Divide[F: Fractional](left: Computation[F], right: Computation[F]) extends Computation[F]
  
  def pure[F: Fractional](p: F): Computation[F] = Pure(p)
  def add[F: Fractional](left: Computation[F], right: Computation[F]): Computation[F] = Add(left, right)
  def subtract[F: Fractional](left: Computation[F], right: Computation[F]): Computation[F] = Subtract(left, right)
  def multiply[F: Fractional](left: Computation[F], right: Computation[F]): Computation[F] = Multiply(left, right)
  def divide[F: Fractional](left: Computation[F], right: Computation[F]): Computation[F] = Divide(left, right)

  def computeWithState[F: Fractional](computation: Computation[F]): ComputationCacheA[F] =
    for {
      maybeResult <- State.get[Map[Computation[F], F]].map(_.get(computation))
      result      <- compute(maybeResult, computation)
    } yield result

  private def compute[F](maybeResult: Option[F], computation: Computation[F])
                        (implicit num: Fractional[F]): ComputationCacheA[F] = {
    def helper(parent: Computation[F], left: Computation[F], right: Computation[F])(f: (F, F) => F): ComputationCacheA[F] =
      for {
        leftResult  <- computeWithState(left)
        rightResult <- computeWithState(right)
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
  
  object syntax {
    implicit class PureSyntax[F: Fractional](val num: F) {
      def n: Computation[F] = Pure(num)
    }

    implicit class AddSyntax[F: Fractional](val left: Computation[F]) {
      def +(right: Computation[F]): Computation[F] = Add(left, right)
    }

    implicit class SubtractSyntax[F: Fractional](val left: Computation[F]) {
      def -(right: Computation[F]): Computation[F] = Subtract(left, right)
    }

    implicit class MultiplySyntax[F: Fractional](val left: Computation[F]) {
      def *(right: Computation[F]): Computation[F] = Multiply(left, right)
    }

    implicit class DivideSyntax[F: Fractional](val left: Computation[F]) {
      def /(right: Computation[F]): Computation[F] = Divide(left, right)
    }
  }
}
