package cats.derived

import shapeless3.deriving.*
import shapeless3.deriving.internals.ErasedInstances

import scala.annotation.*
import scala.compiletime.summonFrom

private[derived] infix type >>>[F[_], G[_]] = [x] =>> G[F[x]]
private[derived] infix type <<<[F[_], G[_]] = [x] =>> F[G[x]]

private[derived] given [I[k, t] <: ErasedInstances[k, t], K, T](using inst: I[K, Derived.Or[T]]): I[K, T] =
  Derived.Or.unify(inst)

@implicitNotFound("Could not derive an instance of ${A}")
opaque type Derived[A] = A
object Derived:
  def apply[A](instance: A): Derived[A] = instance
  given [A]: Conversion[A, Derived[A]] = identity
  extension [A](derived: Derived[A]) def instance: A = derived

  type Or0[F[_]] = [x] =>> Derived.Or[F[x]]
  type Or1[F[_[_]]] = [x[_]] =>> Derived.Or[F[x]]
  type Or11[F[_[_[_]]]] = [x[_[_]]] =>> Derived.Or[F[x]]
  type Or2[F[_[_, _]]] = [x[_, _]] =>> Derived.Or[F[x]]

  opaque type Or[A] = A
  object Or extends OrInstances:
    def apply[A](instance: A): Or[A] = instance
    given [A]: Conversion[Or[A], A] = identity
    extension [A](derived: Or[A]) def unify: A = derived
    extension [I[k, t] <: ErasedInstances[k, t], K, T](inst: I[K, Or[T]])
      @targetName("unifyInstances") def unify: I[K, T] = inst

    @deprecated("Use unifyInstances instead")
    def unifyK0[I[f[_], t] <: K0.Instances[f, t], F[_], T](
        inst: I[Or0[F], T]
    ): I[F, T] = inst

    @deprecated("Use unifyInstances instead")
    def unifyK1[I[f[_[_]], t[_]] <: K1.Instances[f, t], F[_[_]], T[_]](
        inst: I[Or1[F], T]
    ): I[F, T] = inst

    @deprecated("Use unifyInstances instead")
    def unifyK11[I[f[_[_[_]]], t[_[_]]] <: K11.Instances[f, t], F[_[_[_]]], T[_[_]]](
        inst: I[Or11[F], T]
    ): I[F, T] = inst

    @deprecated("Use unifyInstances instead")
    def unifyK2[I[f[_[_, _]], t[_, _]] <: K2.Instances[f, t], F[_[_, _]], T[_, _]](
        inst: I[Or2[F], T]
    ): I[F, T] = inst

  private[derived] open class Lazy[A](f: () => A) extends Serializable:
    final protected lazy val delegate: A = f()

sealed abstract class OrInstances:
  inline given [A]: Derived.Or[A] = summonFrom:
    case instance: A => Derived.Or(instance)
    case derived: Derived[A] => Derived.Or(derived.instance)
