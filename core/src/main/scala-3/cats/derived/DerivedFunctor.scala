package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Functor for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Functor and H: Functor
  * nested type [x] =>> G[H[x]] where G: Contravariant and H: Contravariant
  * generic case class where all fields form Functor
  * generic sealed trait where all subclasses form Functor
  * generic enum where all variants form Functor""")
type DerivedFunctor[F[_]] = Derived[Functor[F]]
object DerivedFunctor:
  @nowarn("msg=unused import")
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Functor[F] =
    import Strict.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using F: => Derived.Or[Functor[F]], G: => Derived.Or[Functor[G]]): DerivedFunctor[F <<< G] =
    new Derived.Lazy(() => F.compose(using G)) with Functor[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      F: Derived.Or[Contravariant[F]],
      G: Derived.Or[Contravariant[G]]
  ): DerivedFunctor[F <<< G] =
    F.compose(using G)

  given [F[_]](using inst: => Instances[Derived.Or1[Functor], F]): DerivedFunctor[F] =
    generic

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: Derived.Or[Functor[F]],
      G: Derived.Or[Functor[G]]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: Derived.Or[Contravariant[F]],
      G: Derived.Or[Contravariant[G]]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  private def generic[F[_]: InstancesOf[Functor]]: DerivedFunctor[F] =
    new Generic[Functor, F] {}

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))

  object Strict:
    given product[F[_]: ProductInstancesOf[Functor]]: DerivedFunctor[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Derived.Or1[Functor], F]): DerivedFunctor[F] = generic
