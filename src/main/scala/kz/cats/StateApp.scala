package kz.cats

object StateApp {

  import Computation._
  import Computation.syntax._

  def main(args: Array[String]): Unit = {
    val computation1 = Add(Pure(2d), Multiply(Add(Pure(2d), Pure(4d)), Add(Pure(2d), Pure(4d))))
    val (cache1, result1) = computeWithState[Double](computation1).run(Map.empty).value
    println(s"Cache: $cache1")
    println(s"Result: $result1")

    val computation2 = add(pure(2d), multiply(add(pure(2d), pure(4d)), add(pure(2d), pure(4d))))
    val (cache2, result2) = computeWithState[Double](computation2).run(Map.empty).value
    println(s"Cache: $cache2")
    println(s"Result: $result2")

    val computation3 = 2d.n + ((2d.n + 4d.n) * (2d.n + 4d.n))
    val (cache3, result3) = computeWithState[Double](computation3).run(Map.empty).value
    println(s"Cache: $cache3")
    println(s"Result: $result3")
  }
}
