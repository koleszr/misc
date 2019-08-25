package kz.cats

sealed trait Computation[F]

object Computation {
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
