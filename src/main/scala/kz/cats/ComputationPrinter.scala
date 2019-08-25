package kz.cats

import kz.cats.Computation._

object ComputationPrinter {

  def show[F: Fractional](computation: Computation[F]): String = {

    def go(computation: Computation[F]): String =
      computation match {
        case Pure(p) => p.toString
        case Add(left, right) => s"${go(left)}+${go(right)}"
        case Subtract(left, right) => s"${go(left)}-${go(right)}"
        case m: Multiply[F] => multiplyHelper(m)
        case d: Divide[F] => divideHelper(d)
      }

    def multiplyHelper(m: Multiply[F]): String = {
      val Multiply(left, right) = m

      val leftString = left match {
        case _: Pure[F] => go(left)
        case _          => s"(${go(left)})"
      }

      val rightString = right match {
        case _: Pure[F] => go(right)
        case _          => s"(${go(right)})"
      }

      s"$leftString*$rightString"
    }

    def divideHelper(d: Divide[F]): String = {
      val Divide(left, right) = d

      val leftString = left match {
        case _: Pure[F] => go(left)
        case _          => s"(${go(left)})"
      }

      val rightString = right match {
        case _: Pure[F] => go(right)
        case _          => s"(${go(right)})"
      }

      s"$leftString/$rightString"
    }

    go(computation)
  }

}
