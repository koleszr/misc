import java.io.IOException

import zio.ZIO

package object kz {

  object errors {
    sealed trait MiscError

    object MiscError {
      final case class IntConversionError(stringToConvert: String) extends MiscError
      final case class IOError(e: IOException) extends MiscError
    }
  }

  object implicits {

    import errors.MiscError
    import errors.MiscError._

    implicit class StringToNumberZio[R](val str: String) extends AnyVal {
      def toIntZio: ZIO[R, MiscError, Int] =
        str.toIntOption match {
          case Some(n) => ZIO.succeed(n)
          case None    => ZIO.fail[MiscError](IntConversionError(str))
        }

      def toDoubleZio: ZIO[R, MiscError, Double] =
        str.toDoubleOption match {
          case Some(d) => ZIO.succeed(d)
          case None    => ZIO.fail[MiscError](IntConversionError(str))
        }
    }
  }
}
