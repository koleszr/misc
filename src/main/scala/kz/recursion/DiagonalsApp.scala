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
            val diagonals: List[Diagonal] = possibleDiagonals(head.table, head.position)        
            val solverMetas: List[SolverMeta] = (None :: diagonals.map(Some(_))).map { diagonal =>
              val table = head.table.zipWithIndex.map { case (row, i1) =>
                row.zipWithIndex.map { 
                  case (_, j1) if i == i1 && j == j1 => diagonal
                  case (elem, _)                     => elem

                }
              }
              val diagonalsLeft = diagonal.map(_ => head.diagonalsLeft - 1).getOrElse(head.diagonalsLeft)
              SolverMeta(table, nextPos, diagonalsLeft, head.cellsLeft - 1)
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

    def inside(m: Int, n: Int, size: Int): Boolean = m >= 0 && n >= 0 && m < size && n < size

    val leftTopDiagonal: Set[Diagonal] =
      if (inside(i - 1, j - 1, size)) {
        table(i - 1)(j - 1) match {
          case Some(LeftToRight) => Set(RightToLeft)
          case Some(RightToLeft) => Set(LeftToRight, RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val topDiagonal: Set[Diagonal] =
      if (inside(i - 1, j, size)) {
        table(i - 1)(j) match {
          case Some(LeftToRight) => Set(LeftToRight)
          case Some(RightToLeft) => Set(RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val rightTopDiagonal: Set[Diagonal] =
      if (inside(i - 1, j + 1, size)) {
        table(i - 1)(j + 1) match {
          case Some(LeftToRight) => Set(LeftToRight, RightToLeft)
          case Some(RightToLeft) => Set(LeftToRight)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val leftDiagonal: Set[Diagonal] =
      if (inside(i, j - 1, size)) {
        table(i)(j - 1) match {
          case Some(LeftToRight) => Set(LeftToRight)
          case Some(RightToLeft) => Set(RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val rightDiagonal: Set[Diagonal] =
      if (inside(i, j + 1, size)) {
        table(i)(j + 1) match {
          case Some(LeftToRight) => Set(LeftToRight)
          case Some(RightToLeft) => Set(RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val leftBottomDiagonal: Set[Diagonal] =
      if (inside(i + 1, j - 1, size)) {
        table(i + 1)(j - 1) match {
          case Some(LeftToRight) => Set(LeftToRight, RightToLeft)
          case Some(RightToLeft) => Set(LeftToRight)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val bottomDiagonal: Set[Diagonal] =
      if (inside(i + 1, j, size)) {
        table(i + 1)(j) match {
          case Some(LeftToRight) => Set(LeftToRight)
          case Some(RightToLeft) => Set(RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    val rightBottomDiagonal: Set[Diagonal] =
      if (inside(i + 1, j + 1, size)) {
        table(i + 1)(j + 1) match {
          case Some(LeftToRight) => Set(RightToLeft)
          case Some(RightToLeft) => Set(LeftToRight, RightToLeft)
          case None              => Set(LeftToRight, RightToLeft)
        }
      } else {
        Set(LeftToRight, RightToLeft)
      }

    leftTopDiagonal
      .intersect(topDiagonal)
      .intersect(rightTopDiagonal)
      .intersect(leftDiagonal)
      .intersect(rightDiagonal)
      .intersect(leftBottomDiagonal)
      .intersect(bottomDiagonal)
      .intersect(rightBottomDiagonal)
      .toList
  }
}
