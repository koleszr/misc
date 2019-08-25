package kz.cats

object StateApp {

  import CachedComputation._
  import Computation._
  import Computation.syntax._
  import ComputationPrinter.show

  def main(args: Array[String]): Unit = {
    cachedMain()
    showMain()
  }

  def cachedMain(): Unit = {
    println("========================================")
    println("=          Cached computation          =")
    println("========================================")
    val computation1 = Add(Pure(2d), Multiply(Add(Pure(2d), Pure(4d)), Add(Pure(2d), Pure(4d))))
    val (cache1, result1) = compute[Double](computation1).run(Map.empty).value
    println(s"Cache: $cache1")
    println(s"Result: $result1")

    val computation2 = add(pure(2d), multiply(add(pure(2d), pure(4d)), add(pure(2d), pure(4d))))
    val (cache2, result2) = compute[Double](computation2).run(Map.empty).value
    println(s"Cache: $cache2")
    println(s"Result: $result2")

    val computation3 = 2d.n + ((2d.n + 4d.n) * (2d.n + 4d.n))
    val (cache3, result3) = compute[Double](computation3).run(Map.empty).value
    println(s"Cache: $cache3")
    println(s"Result: $result3")
  }

  def showMain(): Unit = {
    println("========================================")
    println("=           Show computation           =")
    println("========================================")

    val computation1 = 2d.n
    println(s"Computation 1: ${show(computation1)}")

    val computation2 = 2d.n + 12d.n
    println(s"Computation 2: ${show(computation2)}")

    val computation3 = 2d.n * 12d.n
    println(s"Computation 3: ${show(computation3)}")

    val computation4 = 2d.n + (2d.n + 4d.n) * (2d.n + 4d.n)
    println(s"Computation 4: ${show(computation4)}")

    val computation5 = 2d.n + ((2d.n + 4d.n) * (15d.n - 32d.n)) / ((15d.n * 32d.n) + (42d.n * 42d.n))
    println(s"Computation 5: ${show(computation5)}")
  }
}
