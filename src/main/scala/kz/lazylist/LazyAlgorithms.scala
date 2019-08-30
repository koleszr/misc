package kz.lazylist

object LazyAlgorithms {

  private val factorial: LazyList[BigInt] = 1 #:: factorial.zipWithIndex.map { case (n, i) => n * (i + 1) }

  def fact(n: Int): BigInt = factorial.drop(n).head

  /**
   * Newton-Raphson method using LazyList for approximating square root of {{d}}.
   */
  private class Sqrt(d: Double) {
    val nrSqrt: (Double, Double) => Double = (initialGuess, previous) => 0.5 * (previous + initialGuess / previous)
    val approxSqrt: LazyList[Double] = d #:: approxSqrt.map(previous => nrSqrt(d, previous))
  }

  /**
   * Approximated square root of {{d}}, where two successive values' difference is less than
   * the given tolerance of {{e}}.
   */
  def sqrt(d: Double, e: Double): Double = {
    val s = new Sqrt(d)
    s.approxSqrt.zip(s.approxSqrt.tail).dropWhile { case (pp, p) => Math.abs(pp - p) >= e }.head._2
  }

}
