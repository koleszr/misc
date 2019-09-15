package kz.state

import zio.ZIO
import zio.Ref

trait State[S] {
  val state: State.Service[Any, S]
}

object State {
  trait Service[R, S] {
    def pure[A](a: A): ZIO[R, Nothing, A]
    def set(state: S): ZIO[R, Nothing, Unit]
    def modify(f: S => S): ZIO[R, Nothing, Unit]
    def get: ZIO[R, Nothing, S]
  }

  trait Live[S] extends State[S] {
    def stateRef: Ref[S]

    override val state: State.Service[Any, S] = new State.Service[Any, S] {
      override def pure[A](a: A): ZIO[Any, Nothing, A] = ZIO.succeed(a)
      override def get: ZIO[Any, Nothing, S] = stateRef.get
      override def set(s: S): ZIO[Any, Nothing, Unit] = stateRef.set(s)
      override def modify(f: S => S): ZIO[Any, Nothing, Unit] = stateRef.modify(s => ((), f(s)))
    }
  }
}
