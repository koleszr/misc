package kz.lazylist

import LazyAlgorithms._

object LazyApp {

  def main(args: Array[String]): Unit = {
    val factOfFive = fact(5)
    println(s"5! = $factOfFive")

    val sqrtOf9 = sqrt(9, 0.000001)
    println(s"Square root of 9: $sqrtOf9")
  }
}
