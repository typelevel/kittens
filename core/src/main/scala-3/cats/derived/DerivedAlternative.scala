package cats.derived

import cats.Alternative
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Alternative for ${F}.
Make sure it satisfies one of the following conditions:
  * nested type [x] =>> G[H[x]] where G: Alternative and H: Applicative
  * generic case class where all fields form Alternative""")
type DerivedAlternative[F[_]] = Derived[Alternative[F]]
object DerivedAlternative:
  @nowarn("msg=unused import")
  inline def apply[F[_]]: Alternative[F] =
    import DerivedAlternative.given
    summonInline[DerivedAlternative[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Alternative[F] =
    import Strict.given
    summonInline[DerivedAlternative[F]].instance

  given nested[F[_], G[_]](using
      F: => Derived.Or[Alternative[F]],
      G: => Derived.Or[Alternative[G]]
  ): DerivedAlternative[F <<< G] =
    new Derived.Lazy(() => F.compose(using G)) with Alternative[F <<< G]:
      export delegate.*

  given product[F[_]](using inst: => ProductInstances[Derived.Or1[Alternative], F]): DerivedAlternative[F] =
    Strict.product

  trait Product[T[f[_]] <: Alternative[f], F[_]: ProductInstancesOf[T]]
      extends Alternative[F],
        DerivedNonEmptyAlternative.Product[T, F],
        DerivedMonoidK.Product[T, F]

  object Strict:
    given product[F[_]: ProductInstancesOf[Alternative]]: DerivedAlternative[F] =
      new Alternative[F] with Product[Alternative, F] {}
