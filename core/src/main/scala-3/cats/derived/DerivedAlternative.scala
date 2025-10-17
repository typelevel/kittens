package cats.derived

import cats.Alternative
import shapeless3.deriving.Derived
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Alternative for ${F}.
Make sure it satisfies one of the following conditions:
  * nested type [x] =>> G[H[x]] where G: Alternative and H: Applicative
  * generic case class where all fields form Alternative""")
type DerivedAlternative[F[_]] = Derived[Alternative[F]]
object DerivedAlternative:
  inline def apply[F[_]]: Alternative[F] =
    import DerivedAlternative.given
    summonInline[DerivedAlternative[F]].instance

  inline def strict[F[_]]: Alternative[F] =
    import Strict.given
    summonInline[DerivedAlternative[F]].instance

  given nested[F[_], G[_]](using
      F: => (Alternative |: Derived)[F],
      G: => (Alternative |: Derived)[G]
  ): DerivedAlternative[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Alternative[F <<< G]:
      export delegate.*

  given product[F[_]](using inst: => ProductInstances[Alternative |: Derived, F]): DerivedAlternative[F] =
    Strict.product(using inst.unify)

  trait Product[T[f[_]] <: Alternative[f], F[_]]
      extends Alternative[F],
        DerivedNonEmptyAlternative.Product[T, F],
        DerivedMonoidK.Product[T, F]

  object Strict:
    given product[F[_]: ProductInstancesOf[Alternative]]: DerivedAlternative[F] =
      new Alternative[F] with Product[Alternative, F] {}
