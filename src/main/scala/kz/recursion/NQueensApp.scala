package kz.recursion

import kz.errors.MiscError.IOError
import kz.implicits._

import scala.annotation.tailrec
import zio.{App, ZIO}
import zio.console._

object NQueensApp extends App {

  type Permutation = List[Int]

  override def run(args: List[String]): ZIO[NQueensApp.Environment, Nothing, Int] =
    (for {
      _         <- putStr("Enter the size of the chess table: ")
      n         <- getStrLn.mapError(IOError).flatMap(_.toIntZio)
      solutions =  nQueens(n)
      _         <- putStrLn(s"Solutions of the N-Queen problem: ${solutions.mkString("\n")}")
      _         <- putStrLn(s"Number of solutions of the N-Queen problem: ${solutions.size}")
    } yield ()).fold(_ => 1, _ => 0)

  def nQueens(n: Int): List[Permutation] = {
    def canBeExtendedToSolution(permutation: Permutation): Boolean = {
      val i = permutation.size - 1
      val ithElement = permutation(i)
      (0 until i).forall { j => (i - j) != Math.abs(ithElement - permutation(j)) }
    }

    @tailrec
    def go(toVisit: List[Permutation], visited: List[Permutation] = Nil): List[Permutation] = {
      toVisit match {
        case Nil => visited
        case head :: tail =>
          if (head.size == n) {
            go(tail, head :: visited)
          } else {
            val newPermutationsToVisit = (0 until n)
              .collect { case i if !head.contains(i) => head :+ i }
              .filter(canBeExtendedToSolution)
              .toList
            go(newPermutationsToVisit ::: tail, visited)
          }
      }
    }

    go(List(Nil))
  }
}
