package cats.derived

import alleycats.{Empty, Pure}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.summonInline

@implicitNotFound("""Could not derive Pure for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T where T: Empty
  * nested type [x] =>> G[H[x]] where G: Pure and H: Pure
  * generic case class where all fields form Pure""")
type DerivedPure[F[_]] = Derived[Pure[F]]
object DerivedPure:
  inline def apply[F[_]]: Pure[F] =
    import DerivedPure.given
    summonInline[DerivedPure[F]].instance

  inline def strict[F[_]]: Pure[F] =
    import Strict.given
    summonInline[DerivedPure[F]].instance

  given [T](using T: Empty[T]): DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = T.empty

  given [T <: Singleton: ValueOf]: DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = valueOf[T]

  given nested[F[_], G[_]](using F: => (Pure |: Derived)[F], G: => (Pure |: Derived)[G]): DerivedPure[F <<< G] =
    new Pure[F <<< G]:
      lazy val f = F.unify
      lazy val g = G.unify
      def pure[A](a: A): F[G[A]] = f.pure(g.pure(a))

  given [F[_]](using inst: ProductInstances[Pure |: Derived, F]): DerivedPure[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Pure |: Derived, G[_]: Pure |: Derived]: DerivedPure[[x] =>> F[G[x]]] = nested

  object Strict:
    given product[F[_]: ProductInstancesOf[Pure]]: DerivedPure[F] = new Pure[F]:
      def pure[A](a: A): F[A] = ProductInstances.construct([f[_]] => (F: Pure[f]) => F.pure(a))
