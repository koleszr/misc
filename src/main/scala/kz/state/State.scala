package kz.state

import zio.{Ref, UIO, ZIO}

trait State[S] {
  val state: State.Service[S]
}

object State {
  trait Service[S] {
    def pure[A](a: A): UIO[A]
    def get: UIO[S]
    def set(state: S): UIO[Unit]
    def modify(f: S => S): UIO[Unit]
  }

  trait Live[S] extends State[S] {
    def stateRef: Ref[S]

    override val state: State.Service[S] = new State.Service[S] {
      override def pure[A](a: A): UIO[A] = ZIO.succeed(a)
      override def get: UIO[S] = stateRef.get
      override def set(s: S): UIO[Unit] = stateRef.set(s)
      override def modify(f: S => S): UIO[Unit] = stateRef.modify(s => ((), f(s)))
    }
  }
}
