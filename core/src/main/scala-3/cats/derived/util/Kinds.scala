package cats.derived.util

import shapeless3.deriving._

import scala.compiletime.*
import scala.util.NotGiven

object Kinds:
  inline def summonOne0[F[_], T, U]: F[U] =
    summonOneFrom[K0.LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonOne1[F[_[_]], T[_], U[_]]: F[U] =
    summonOneFrom[K1.LiftP[F, T]].asInstanceOf[F[U]]

  inline def summonNone0[F[_], T]: Unit =
    summonNoneFrom[K0.LiftP[F, T]]

  inline def summonNone1[F[_[_]], T[_]]: Unit =
    summonNoneFrom[K1.LiftP[F, T]]

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
