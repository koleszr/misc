package kz.lazylist

object LazyAlgorithms {

  private val factorial: LazyList[BigInt] = 1 #:: factorial.zipWithIndex.map { case (n, i) => n * (i + 1) }

  def fact(n: Int): BigInt = factorial.drop(n).head

  private val outside: (Double, Double, Double) => Boolean = (d1, d2, e) => Math.abs(d1 - d2) >= e

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
    s.approxSqrt.zip(s.approxSqrt.tail).dropWhile { case (pp, p) => outside(pp, p, e) }.head._2
  }

  /**
   * Approximated derivative of {{f}} at {{x}}.
   */
  private class Derivative(x: Double, f: Double => Double) {
    private val simpleDerivative: Double => (Double, Double) = h => ((f(x + h) - f(x)) / h, h)

    val approxDerivative: LazyList[(Double, Double)] =
      simpleDerivative(0.1) #:: approxDerivative.map { case (_, h) => simpleDerivative(h / 2) }
  }

  def derivative(x: Double, e: Double)(f: Double => Double): Double = {
    val d = new Derivative(x, f)
    d.approxDerivative.zip(d.approxDerivative.tail).dropWhile { case ((pp, _), (p, _)) => outside(pp, p, e) }.head._2._1
  }

}
