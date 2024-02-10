package cats.derived

import cats.derived.Derived.<<<
import cats.{Applicative, Monoid}
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Applicative for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T where T: Monoid
  * nested type [x] =>> G[H[x]] where G: Applicative and H: Applicative
  * generic case class where all fields form Applicative""")
type DerivedApplicative[F[_]] = Derived[Applicative[F]]
object DerivedApplicative:
  type Or[F[_]] = Derived.Or[Applicative[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Applicative[F] =
    import DerivedApplicative.given
    summonInline[DerivedApplicative[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Applicative[F] =
    import Strict.given
    summonInline[DerivedApplicative[F]].instance

  given [T](using T: Monoid[T]): DerivedApplicative[Const[T]] = new Applicative[Const[T]]:
    def pure[A](x: A): T = T.empty
    def ap[A, B](ff: T)(fa: T): T = T.combine(ff, fa)

  given nested[F[_], G[_]](using
      F: => DerivedApplicative.Or[F],
      G: => DerivedApplicative.Or[G]
  ): DerivedApplicative[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Applicative[F <<< G]:
      export delegate.*

  given [F[_]](using inst: => ProductInstances[Or, F]): DerivedApplicative[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: DerivedApplicative.Or, G[_]: DerivedApplicative.Or]: DerivedApplicative[[x] =>> F[G[x]]] =
    nested

  trait Product[T[f[_]] <: Applicative[f], F[_]](using inst: ProductInstances[T, F])
      extends Applicative[F],
        DerivedApply.Product[T, F]:

    final override def pure[A](x: A): F[A] =
      inst.construct([f[_]] => (F: T[f]) => F.pure[A](x))

  object Strict:
    given product[F[_]: ProductInstancesOf[Applicative]]: DerivedApplicative[F] =
      new Applicative[F] with Product[Applicative, F] {}
