package kz.cats

import kz.computation.Computation
import kz.computation.Computation._

object ComputationPrinter {

  def show[F: Fractional](computation: Computation[F]): String = {

    def go(computation: Computation[F]): String =
      computation match {
        case Pure(p)               => p.toString
        case Add(left, right)      => s"${go(left)}+${go(right)}"
        case Subtract(left, right) => s"${go(left)}-${go(right)}"
        case m: Multiply[F]        => multiplyHelper(m)
        case d: Divide[F]          => divideHelper(d)
      }

    def helper(computation: Computation[F]): String =
      computation match {
        case _: Pure[F] => go(computation)
        case _          => s"(${go(computation)})"
      }

    def multiplyHelper(m: Multiply[F]): String = {
      val Multiply(left, right) = m
      s"${helper(left)}*${helper(right)}"
    }

    def divideHelper(d: Divide[F]): String = {
      val Divide(left, right) = d
      s"${helper(left)}/${helper(right)}"
    }

    go(computation)
  }

}
