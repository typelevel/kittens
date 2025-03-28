package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Contravariant for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T
  * nested type [x] =>> G[H[x]] where G: Functor and H: Contravariant
  * generic case class where all fields form Contravariant
  * generic sealed trait where all subclasses form Contravariant
  * generic enum where all variants form Contravariant""")
type DerivedContravariant[F[_]] = Derived[Contravariant[F]]
object DerivedContravariant:
  @nowarn("msg=unused import")
  inline def apply[F[_]]: Contravariant[F] =
    import DerivedContravariant.given
    summonInline[DerivedContravariant[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Contravariant[F] =
    import Strict.given
    summonInline[DerivedContravariant[F]].instance

  given [T]: DerivedContravariant[Const[T]] = new Contravariant[Const[T]]:
    def contramap[A, B](fa: T)(f: B => A): T = fa

  given nested[F[_], G[_]](using
      F: (Functor |: Derived)[F],
      G: => (Contravariant |: Derived)[G]
  ): DerivedContravariant[F <<< G] =
    new Lazy(() => F.unify.composeContravariant(using G.unify)) with Contravariant[F <<< G]:
      export delegate.*

  given [F[_]](using inst: => Instances[Contravariant |: Derived, F]): DerivedContravariant[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Functor |: Derived, G[_]: Contravariant |: Derived]: DerivedContravariant[[x] =>> F[G[x]]] =
    nested

  private def generic[F[_]: InstancesOf[Contravariant]]: DerivedContravariant[F] =
    new Generic[Contravariant, F] {}

  trait Generic[T[f[_]] <: Contravariant[f], F[_]](using inst: Instances[T, F]) extends Contravariant[F]:
    final override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
      inst.map(fa)([f[_]] => (T: T[f], fa: f[A]) => T.contramap(fa)(f))

  object Strict:
    given product[F[_]: ProductInstancesOf[Contravariant]]: DerivedContravariant[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Contravariant |: Derived, F]): DerivedContravariant[F] =
      generic(using inst.unify)
