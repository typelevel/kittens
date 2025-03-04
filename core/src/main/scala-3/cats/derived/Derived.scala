package cats.derived

import shapeless3.deriving.*

@deprecated("Replaced by shapeless3.deriving.Derived", "3.5.0")
private[derived] opaque type Derived[A] = A

@deprecated("Replaced by shapeless3.deriving.Derived", "3.5.0")
private[derived] object Derived:
  def apply[A](instance: A): Derived[A] = instance
  given [A]: Conversion[A, Derived[A]] = identity
  extension [A](derived: Derived[A]) def instance: A = derived

  @deprecated("Replaced by shapeless3.deriving.OrElse", "3.5.0")
  opaque type Or[A] = A

  @deprecated("Replaced by shapeless3.deriving.OrElse", "3.5.0")
  object Or extends OrInstances:
    def apply[A](instance: A): Or[A] = instance
    extension [A](derived: Or[A]) def unify: A = derived
    def unifyK0[I[f[_], t] <: K0.Instances[f, t], F[_], T](inst: I[F, T]): I[F, T] = inst
    def unifyK1[I[f[_[_]], t[_]] <: K1.Instances[f, t], F[_[_]], T[_]](inst: I[F, T]): I[F, T] = inst
    def unifyK11[I[f[_[_[_]]], t[_[_]]] <: K11.Instances[f, t], F[_[_[_]]], T[_[_]]](inst: I[F, T]): I[F, T] = inst
    def unifyK2[I[f[_[_, _]], t[_, _]] <: K2.Instances[f, t], F[_[_, _]], T[_, _]](inst: I[F, T]): I[F, T] = inst

@deprecated("Replaced by shapeless3.deriving.OrElse", "3.5.0")
sealed abstract private[derived] class OrInstances
