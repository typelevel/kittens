package cats.derived

import alleycats.{Empty, EmptyK, Pure}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.summonInline
import scala.util.NotGiven

@implicitNotFound("""Could not derive EmptyK for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T where T: Empty
  * nested type [x] =>> G[H[x]] where G: EmptyK
  * nested type [x] =>> G[H[x]] where G: Pure and H: EmptyK
  * generic case class where all fields form EmptyK
  * generic sealed trait where exactly one subclass forms EmptyK
  * generic enum where exactly one variant forms EmptyK""")
type DerivedEmptyK[F[_]] = Derived[EmptyK[F]]
object DerivedEmptyK:
  inline def apply[F[_]]: EmptyK[F] =
    import DerivedEmptyK.given
    summonInline[DerivedEmptyK[F]].instance

  inline def strict[F[_]]: EmptyK[F] =
    import Strict.given
    summonInline[DerivedEmptyK[F]].instance

  given [T](using T: Empty[T]): DerivedEmptyK[Const[T]] =
    new EmptyK[Const[T]]:
      def empty[A]: T = T.empty

  given nested[F[_], G[_]](using F: => (EmptyK |: Derived)[F]): DerivedEmptyK[F <<< G] =
    new EmptyK[F <<< G]:
      lazy val f = F.unify
      def empty[A]: F[G[A]] = f.empty

  given nested[F[_], G[_]](using
      NotGiven[(EmptyK |: Derived)[F]]
  )(using F: (Pure |: Derived)[F], G: => (EmptyK |: Derived)[G]): DerivedEmptyK[F <<< G] =
    new EmptyK[F <<< G]:
      val f = F.unify
      lazy val g = G.unify
      def empty[A]: F[G[A]] = f.pure(g.empty)

  given product[F[_]](using inst: ProductInstances[EmptyK |: Derived, F]): DerivedEmptyK[F] =
    Strict.product(using inst.unify)

  inline given coproduct[F[_]: CoproductGeneric]: DerivedEmptyK[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: (EmptyK |: Derived)[F]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[(EmptyK |: Derived)[F]]
  )(using (Pure |: Derived)[F], (EmptyK |: Derived)[G]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using ev)

  object Strict:
    given product[F[_]: ProductInstancesOf[EmptyK]]: DerivedEmptyK[F] = new EmptyK[F]:
      def empty[A]: F[A] = ProductInstances.construct([f[_]] => (F: EmptyK[f]) => F.empty[A])

    @nowarn("id=E197")
    inline given coproduct[F[_]: CoproductGeneric]: DerivedEmptyK[F] =
      CoproductGeneric.withOnly[EmptyK |: Derived, EmptyK[F]]: [f[x] <: F[x]] =>
        (F: (EmptyK |: Derived)[f]) => F.asInstanceOf[EmptyK[F]]
