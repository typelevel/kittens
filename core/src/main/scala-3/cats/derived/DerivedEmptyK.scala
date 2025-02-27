package cats.derived

import alleycats.{Empty, EmptyK, Pure}
import shapeless3.deriving.Const
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
  @nowarn("msg=unused import")
  inline def apply[F[_]]: EmptyK[F] =
    import DerivedEmptyK.given
    summonInline[DerivedEmptyK[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: EmptyK[F] =
    import Strict.given
    summonInline[DerivedEmptyK[F]].instance

  given [T](using T: Empty[T]): DerivedEmptyK[Const[T]] =
    new EmptyK[Const[T]]:
      def empty[A]: T = T.empty

  given nested[F[_], G[_]](using F: => Derived.Or[EmptyK[F]]): DerivedEmptyK[F <<< G] =
    new EmptyK[F <<< G]:
      lazy val f = F
      def empty[A]: F[G[A]] = f.empty

  given nested[F[_], G[_]](using
      NotGiven[Derived.Or[EmptyK[F]]]
  )(using F: Derived.Or[Pure[F]], G: => Derived.Or[EmptyK[G]]): DerivedEmptyK[F <<< G] =
    new EmptyK[F <<< G]:
      lazy val g = G
      def empty[A]: F[G[A]] = F.pure(g.empty)

  given product[F[_]: ProductInstancesOf[Derived.Or1[EmptyK]]]: DerivedEmptyK[F] =
    Strict.product

  inline given coproduct[F[_]: CoproductGeneric]: DerivedEmptyK[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Derived.Or[EmptyK[F]]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[Derived.Or[EmptyK[F]]]
  )(using Derived.Or[Pure[F]], Derived.Or[EmptyK[G]]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using ev)

  object Strict:
    given product[F[_]: ProductInstancesOf[EmptyK]]: DerivedEmptyK[F] = new EmptyK[F]:
      def empty[A]: F[A] = ProductInstances.construct([f[_]] => (F: EmptyK[f]) => F.empty[A])

    @nowarn("id=E197")
    inline given coproduct[F[_]: CoproductGeneric]: DerivedEmptyK[F] =
      CoproductGeneric.withOnly[Derived.Or1[EmptyK], EmptyK[F]]:
        [f[x] <: F[x]] => (F: Derived.Or[EmptyK[f]]) => F.asInstanceOf[EmptyK[F]]
