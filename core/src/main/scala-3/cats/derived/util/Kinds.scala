package cats.derived.util

import shapeless3.deriving.K0.LiftP

import scala.compiletime.*
import scala.util.NotGiven

object Kinds:
  inline def summonOne0[F[_], T, U]: F[U] =
    summonOneFrom[LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonNone0[F[_], T]: Unit =
    summonNoneFrom[LiftP[F, T]]

  transparent inline def summonOneFrom[T <: Tuple]: Any =
    inline erasedValue[T] match
      case _: (a *: b) =>
        summonFrom {
          case instance: `a` =>
            summonNoneFrom[b]
            instance
          case _ =>
            summonOneFrom[b]
        }

  transparent inline def summonNoneFrom[T <: Tuple]: Unit =
    inline erasedValue[T] match
      case _: EmptyTuple => ()
      case _: (a *: b) =>
        summonInline[NotGiven[`a`]]
        summonNoneFrom[b]
