package kz.cats

import cats.data.State

import scala.language.higherKinds

sealed trait Computation

object Computation {
  type ComputationCacheA[A] = State[Map[Computation, Int], A]

  final case class Pure(p: Int) extends Computation
  final case class Add(a: Computation, b: Computation) extends Computation
  final case class Subtract(a: Computation, b: Computation) extends Computation
  final case class Multiply(a: Computation, b: Computation) extends Computation
  final case class Divide(a: Computation, b: Computation) extends Computation

  def computeWithState(computation: Computation): ComputationCacheA[Int] =
    for {
      maybeResult <- getFromCache(computation)
      result      <- compute(maybeResult, computation)
    } yield result

  private def compute(maybeResult: Option[Int], computation: Computation): ComputationCacheA[Int] = {
    def helper(parent: Computation, a: Computation, b: Computation)(f: (Int, Int) => Int): ComputationCacheA[Int] =
      for {
        resultA <- computeWithState(a)
        resultB <- computeWithState(b)
        result  =  f(resultA, resultB)
        _       <- State.modify[Map[Computation, Int]](cache => cache + (parent -> result))
      } yield result


    maybeResult match {
      case Some(result) => State.pure(result)
      case None =>
        computation match {
          case Pure(p)                   => State.pure(p)
          case add @ Add(a, b)           => helper(add, a, b)(_ + _)
          case subtract @ Subtract(a, b) => helper(subtract, a, b)(_ - _)
          case multiply @ Multiply(a, b) => helper(multiply, a, b)(_ * _)
          case divide @ Divide(a, b)     => helper(divide, a, b)(_ / _)
        }
    }
  }
  private def getFromCache(computation: Computation): ComputationCacheA[Option[Int]] =
    State.get[Map[Computation, Int]].map(_.get(computation))
}

object StateApp {

  import Computation._

  def main(args: Array[String]): Unit = {
    val computation = Add(Pure(2), Multiply(Add(Pure(2), Pure(4)), Add(Pure(2), Pure(4))))
    val (cache, result) = computeWithState(computation).run(Map.empty).value

    println(s"Cache built: $cache")
    println(s"Result: $result")
  }
}
