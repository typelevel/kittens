package cats.derived

import shapeless3.deriving.*

import scala.annotation.*
import scala.compiletime.summonFrom

@implicitNotFound("Could not derive an instance of ${A}")
opaque type Derived[A] = A
object Derived:
  def apply[A](instance: A): Derived[A] = instance
  extension [A](derived: Derived[A]) def instance: A = derived
  given [A]: Conversion[A, Derived[A]] = apply

  infix type >>>[F[_], G[_]] = [x] =>> G[F[x]]
  infix type <<<[F[_], G[_]] = [x] =>> F[G[x]]

  type Or0[F[_]] = [x] =>> Derived.Or[F[x]]
  type Or1[F[_[_]]] = [x[_]] =>> Derived.Or[F[x]]
  type Or11[F[_[_[_]]]] = [x[_[_]]] =>> Derived.Or[F[x]]
  type Or2[F[_[_, _]]] = [x[_, _]] =>> Derived.Or[F[x]]

  opaque type Or[A] = A
  object Or extends OrInstances:
    def apply[A](instance: A): Or[A] = instance
    extension [A](derived: Or[A]) def unify: A = derived
    extension [I[f[_], t] <: K0.Instances[f, t], F[_], T](inst: I[Or0[F], T])
      @targetName("unifyK0") def unify: I[F, T] = inst
    extension [I[f[_[_]], t[_]] <: K1.Instances[f, t], F[_[_]], T[_]](inst: I[Or1[F], T])
      @targetName("unifyK1") def unify: I[F, T] = inst
    extension [I[f[_[_[_]]], t[_[_]]] <: K11.Instances[f, t], F[_[_[_]]], T[_[_]]](inst: I[Or11[F], T])
      @targetName("unifyK11") def unify: I[F, T] = inst
    extension [I[f[_[_, _]], t[_, _]] <: K2.Instances[f, t], F[_[_, _]], T[_, _]](inst: I[Or2[F], T])
      @targetName("unifyK2") def unify: I[F, T] = inst

  private[derived] open class Lazy[A](f: () => A) extends Serializable:
    final protected lazy val delegate: A = f()

sealed abstract class OrInstances:
  inline given [A]: Derived.Or[A] = summonFrom:
    case instance: A => Derived.Or(instance)
    case derived: Derived[A] => Derived.Or(derived.instance)
