package cats.derived

import cats.NonEmptyAlternative
import cats.derived.Derived.<<<
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive NonEmptyAlternative for ${F}.
Make sure it satisfies one of the following conditions:
  * nested type [x] =>> G[H[x]] where G: NonEmptyAlternative and H: Applicative
  * generic case class where all fields form NonEmptyAlternative""")
type DerivedNonEmptyAlternative[F[_]] = Derived[NonEmptyAlternative[F]]
object DerivedNonEmptyAlternative:
  type Or[F[_]] = Derived.Or[NonEmptyAlternative[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: NonEmptyAlternative[F] =
    import DerivedNonEmptyAlternative.given
    summonInline[DerivedNonEmptyAlternative[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: NonEmptyAlternative[F] =
    import Strict.given
    summonInline[DerivedNonEmptyAlternative[F]].instance

  given nested[F[_], G[_]](using
      F: => DerivedNonEmptyAlternative.Or[F],
      G: => DerivedApplicative.Or[G]
  ): DerivedNonEmptyAlternative[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with NonEmptyAlternative[F <<< G]:
      export delegate.*

  given product[F[_]](using inst: => ProductInstances[Or, F]): DerivedNonEmptyAlternative[F] =
    Strict.product(using inst.unify)

  trait Product[T[f[_]] <: NonEmptyAlternative[f], F[_]: ProductInstancesOf[T]]
      extends NonEmptyAlternative[F],
        DerivedApplicative.Product[T, F],
        DerivedSemigroupK.Product[T, F]

  object Strict:
    given product[F[_]: ProductInstancesOf[NonEmptyAlternative]]: DerivedNonEmptyAlternative[F] =
      new NonEmptyAlternative[F] with Product[NonEmptyAlternative, F] {}
