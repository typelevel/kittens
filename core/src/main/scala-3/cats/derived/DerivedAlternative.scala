package cats.derived

import cats.Alternative
import cats.derived.Derived.<<<
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Alternative[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type [x] =>> G[H[x]] where G: Alternative and H: Applicative
  * it is a generic case class where all fields have a Alternative instance""")
type DerivedAlternative[F[_]] = Derived[Alternative[F]]
object DerivedAlternative:
  type Or[F[_]] = Derived.Or[Alternative[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Alternative[F] =
    import DerivedAlternative.given
    summonInline[DerivedAlternative[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Alternative[F] =
    import Strict.given
    summonInline[DerivedAlternative[F]].instance

  given nested[F[_], G[_]](using
      F: => DerivedAlternative.Or[F],
      G: => DerivedApplicative.Or[G]
  ): DerivedAlternative[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Alternative[F <<< G]:
      export delegate.*

  given product[F[_]](using inst: => ProductInstances[Or, F]): DerivedAlternative[F] =
    Strict.product(using inst.unify)

  trait Product[T[f[_]] <: Alternative[f], F[_]: ProductInstancesOf[T]]
      extends Alternative[F],
        DerivedNonEmptyAlternative.Product[T, F],
        DerivedMonoidK.Product[T, F]

  object Strict:
    given product[F[_]: ProductInstancesOf[Alternative]]: DerivedAlternative[F] =
      new Alternative[F] with Product[Alternative, F] {}
