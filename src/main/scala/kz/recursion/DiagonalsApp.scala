package kz.recursion

import kz.errors.MiscError.IOError
import kz.implicits._

import zio.{App, ZIO}
import zio.console._

sealed trait Diagonal
object Diagonal {
  case object LeftToRight extends Diagonal
  case object RightToLeft extends Diagonal
}

object DiagonalsApp extends App {

  import Diagonal._

  type Table = List[List[Option[Diagonal]]]
  type Position = (Int, Int)

  def show(table: Table): String =
    table.map { row => 
      row.map {
        case None => "0"
        case Some(LeftToRight) => "\\"
        case Some(RightToLeft) => "/"
      }.mkString(" ")
    }.mkString("\n")

  final case class SolverMeta(table: Table, position: Position, diagonalsLeft: Int, cellsLeft: Int)

  override def run(args: List[String]): ZIO[DiagonalsApp.Environment, Nothing, Int] =
    (for {
      _         <- putStr("Enter the size of the table: ")
      n         <- getStrLn.mapError(IOError).flatMap(_.toIntZio)      
      
      _         <- putStr("Enter the number of diagonal elements: ")
      diagonals <- getStrLn.mapError(IOError).flatMap(_.toIntZio)

      table     =  createTable(n)
      solutions =  solve(table, diagonals)

      _         <- putStrLn(s"Solutions: ${solutions.map(show).mkString("\n", "\n\n", "")}")
      _         <- putStrLn(s"Number of solutions: ${solutions.size}")
    } yield ()).fold(_ => 1, _ => 0)

  private def solve(table: Table, left: Int): List[Table] = {
    val size = table.size
    def go(toVisit: List[SolverMeta], visited: List[Table] = Nil): List[Table] = {
      toVisit match {
        case Nil => visited
        case head :: tail =>
          if (head.diagonalsLeft > head.cellsLeft) {
            go(tail, visited)
          } else if (head.diagonalsLeft == 0) {
            go(tail, head.table :: visited)
          } else {
            val (i, j) = head.position
            val nextPos = nextPosition(head.position, size)
            val solverMetas: List[SolverMeta] = 
              (None :: possibleDiagonals(head.table, head.position).map(Some(_))).map { diagonal =>
                SolverMeta(
                  table = head.table.zipWithIndex.map { case (row, i1) =>
                    row.zipWithIndex.map {
                      case (_, j1) if i == i1 && j == j1 => diagonal
                      case (elem, _)                     => elem
                    }
                  },
                  position = nextPos,
                  diagonalsLeft = diagonal.map(_ => head.diagonalsLeft - 1).getOrElse(head.diagonalsLeft),
                  cellsLeft = head.cellsLeft - 1
                )
              }

            go(solverMetas ::: tail, visited)
          }
      }
    }

    go(List(SolverMeta(table, (0, 0), left, size * size)))
  }

  private def nextPosition(position: Position, size: Int): Position = {
    val (i, j) = position
    if (j + 1 == size) (i + 1, 0) else (i, j + 1)
  }

  private def createTable(n: Int): Table = List.fill(n)(List.fill(n)(None))

  private def possibleDiagonals(table: Table, position: Position): List[Diagonal] = {
    val (i, j) = position
    val size = table.size

    def isInside(m: Int, n: Int, size: Int): Boolean = m >= 0 && n >= 0 && m < size && n < size

    def diagonals(i: Int, j: Int)(f: Option[Diagonal] => Set[Diagonal]): Set[Diagonal] =
      if (isInside(i, j, size)) f(table(i)(j)) else Set(LeftToRight, RightToLeft)

    val leftTopDiagonal: Set[Diagonal] = 
      diagonals(i - 1, j - 1) {          
        case Some(LeftToRight) => Set(RightToLeft)
        case Some(RightToLeft) => Set(LeftToRight, RightToLeft)
        case None              => Set(LeftToRight, RightToLeft)
      }

    val topDiagonal: Set[Diagonal] =
      diagonals(i - 1, j) {
        case Some(LeftToRight) => Set(LeftToRight)
        case Some(RightToLeft) => Set(RightToLeft)
        case None              => Set(LeftToRight, RightToLeft)
      }

    val rightTopDiagonal: Set[Diagonal] =
      diagonals(i - 1, j + 1) {
        case Some(LeftToRight) => Set(LeftToRight, RightToLeft)
        case Some(RightToLeft) => Set(LeftToRight)
        case None              => Set(LeftToRight, RightToLeft)
      }


    val leftDiagonal: Set[Diagonal] =
      diagonals(i, j - 1) {
        case Some(LeftToRight) => Set(LeftToRight)
        case Some(RightToLeft) => Set(RightToLeft)
        case None              => Set(LeftToRight, RightToLeft)
      }

    val rightDiagonal: Set[Diagonal] =
      diagonals(i, j + 1) {
        case Some(LeftToRight) => Set(LeftToRight)
        case Some(RightToLeft) => Set(RightToLeft)
        case None              => Set(LeftToRight, RightToLeft)
      }

    leftTopDiagonal
      .intersect(topDiagonal)
      .intersect(rightTopDiagonal)
      .intersect(leftDiagonal)
      .intersect(rightDiagonal)
      .toList
  }
}
