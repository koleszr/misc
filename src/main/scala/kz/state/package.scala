package kz.state

import zio.Ref
import zio.ZIO

package object state {
  def apply[S](stateRef: Ref[S]): State[S] = new State.StateLiveImpl(stateRef)

  def pure[S, A](a: A): ZIO[State[S], Nothing, A] = ZIO.accessM(_.state pure a)
  def set[S](state: S): ZIO[State[S], Nothing, Unit] = ZIO.accessM(_.state set state)
  def modify[S](f: S => S): ZIO[State[S], Nothing, Unit] = ZIO.accessM(_.state modify f)
  def get[S]: ZIO[State[S], Nothing, S] = ZIO.accessM(_.state.get)
}
