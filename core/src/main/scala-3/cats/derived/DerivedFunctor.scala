package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, Derived}
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
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  inline def strict[F[_]]: Functor[F] =
    import Strict.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using
      F: => (Functor |: Derived)[F],
      G: => (Functor |: Derived)[G]
  ): DerivedFunctor[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Functor[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      F: (Contravariant |: Derived)[F],
      G: (Contravariant |: Derived)[G]
  ): DerivedFunctor[F <<< G] =
    F.unify.compose(using G.unify)

  given [F[_]](using inst: => Instances[Functor |: Derived, F]): DerivedFunctor[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: (Functor |: Derived)[F],
      G: (Functor |: Derived)[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: (Contravariant |: Derived)[F],
      G: (Contravariant |: Derived)[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  private def generic[F[_]: InstancesOf[Functor]]: DerivedFunctor[F] =
    new Generic[Functor, F] {}

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))

  object Strict:
    given product[F[_]: ProductInstancesOf[Functor]]: DerivedFunctor[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Functor |: Derived, F]): DerivedFunctor[F] =
      generic(using inst.unify)
