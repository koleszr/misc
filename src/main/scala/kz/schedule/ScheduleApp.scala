package kz.schedule

import java.time.OffsetDateTime

import zio.clock._
import zio.clock.Clock
import zio.console._
import zio.duration._
import zio.{App, Queue, Schedule, ZIO, ZSchedule}

object ScheduleApp extends App {

  import kz.errors._
  import kz.errors.MiscError._
  import kz.implicits._

  override def run(args: List[String]): ZIO[ScheduleApp.Environment, Nothing, Int] =
    (for {
      _ <- dataTimePrinterProgram
      _ <- queueProgram
    } yield ()).fold(_ => 1, _ => 0)

  val dateTimePolicy: ZSchedule[Console, OffsetDateTime, Int] =
    Schedule.recurs(10) <*
      Schedule.spaced(1.second) <*
      ZSchedule.logInput[Console, OffsetDateTime](a => putStrLn(s"Current date-time is $a"))

  val dataTimePrinterProgram: ZIO[Console with Clock, Nothing, Unit] = {
    for {
      _ <- putStrLn("Start of scheduling")
      _ <- currentDateTime repeat dateTimePolicy
      _ <- putStr("End of scheduling")
    } yield ()
  }

  val queueOfferPolicy: Schedule[Any, Int] = Schedule.recurs(10)

  val getStrLnRetryPolicy: Schedule[MiscError, MiscError] =
    Schedule.doWhile[MiscError](_.isInstanceOf[MiscError.IntConversionError])

  val queueProgram: ZIO[Console with Clock, MiscError, Unit] =
    for {
      queue       <- Queue.bounded[Int](20)
      qSizeBefore <- queue.size
      _           <- putStrLn(s"Size of the queue before scheduling: $qSizeBefore")
      _           <- addToQueue(queue) repeat queueOfferPolicy
      qSizeAfter  <- queue.size
      _           <- putStrLn(s"Size of the queue after scheduling: $qSizeAfter")
      _           <- putStrLn(s"Elements of the queue:")
      elements    <- queue.takeAll
      _           <- putStrLn(elements.mkString("[", ", ", "]"))
    } yield ()

  val readIntegerProgram: ZIO[Console, MiscError, Int] =
    putStr("Enter an integer number: ") *> getStrLn.mapError(IOError).flatMap(_.toIntZio)

  def addToQueue(q: Queue[Int]): ZIO[Console with Clock, MiscError, Unit] =
    for {
      i       <- readIntegerProgram retry getStrLnRetryPolicy
      success <- q.offer(i)
      _       <- putStrLn(s"$i was enqueued ${if (success) "successfully" else "unsuccessfully"}!")
    } yield ()
}
