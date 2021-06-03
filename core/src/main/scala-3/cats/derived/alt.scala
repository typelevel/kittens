package cats.derived

import shapeless3.deriving.*
import scala.annotation.*
import scala.compiletime.*

opaque type Alt[A, B] = A | B
object Alt extends AltInstances:
  type Of0[F[_], G[_]] = [x] =>> Alt[F[x], G[x]]
  type Of1[F[_[_]], G[_[_]]] = [x[_]] =>> Alt[F[x], G[x]]
  type Of11[F[_[_[_]]], G[_[_[_]]]] = [x[_[_]]] =>> Alt[F[x], G[x]]
  type Of2[F[_[_, _]], G[_[_, _]]] = [x[_, _]] =>> Alt[F[x], G[x]]

  def primary[A, B](a: A): Alt[A, B] = a
  def secondary[A, B](b: B): Alt[A, B] = b

  extension [A, B](alt: Alt[A, B]) def unify: A | B = alt
  extension [I[f[_], t] <: K0.Instances[f, t], F[_], G[x] <: F[x], T]
    (inst: I[Of0[F, G], T]) @targetName("unifyK0") def unify: I[F, T] = inst
  extension [I[f[_[_]], t[_]] <: K1.Instances[f, t], F[_[_]], G[x[_]] <: F[x], T[_]]
    (inst: I[Of1[F, G], T]) @targetName("unifyK1") def unify: I[F, T] = inst
  extension [I[f[_[_[_]]], t[_[_]]] <: K11.Instances[f, t], F[_[_[_]]], G[x[_[_]]] <: F[x], T[_[_]]]
    (inst: I[Of11[F, G], T]) @targetName("unifyK11") def unify: I[F, T] = inst
  extension [I[f[_[_, _]], t[_, _]] <: K2.Instances[f, t], F[_[_, _]], G[x[_, _]] <: F[x], T[_, _]]
    (inst: I[Of2[F, G], T]) @targetName("unifyK2") def unify: I[F, T] = inst

private[derived] sealed abstract class AltInstances:
  inline given [A, B]: Alt[A, B] = summonFrom {
    case a: A => Alt.primary(a)
    case b: B => Alt.secondary(b)
  }
