package cats.derived

import alleycats.{Empty, EmptyK}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.summonInline
import scala.util.NotGiven

@implicitNotFound("""Could not derive an instance of EmptyK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Empty
  * it is a nested type [x] =>> G[H[x]] where G: EmptyK
  * it is a nested type [x] =>> G[H[x]] where G: Pure and H: EmptyK
  * it is a generic case class where all fields have an EmptyK instance
  * it is a generic sealed trait where exactly one subclass has an EmptyK instance""")
type DerivedEmptyK[F[_]] = Derived[EmptyK[F]]
object DerivedEmptyK:
  type Or[F[_]] = Derived.Or[EmptyK[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: EmptyK[F] =
    import DerivedEmptyK.given
    summonInline[DerivedEmptyK[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: EmptyK[F] =
    import Strict.given
    summonInline[DerivedEmptyK[F]].instance

  given [T](using T: Empty[T]): DerivedEmptyK[Const[T]] = new EmptyK[Const[T]]:
    def empty[A]: T = T.empty

  given nested[F[_], G[_]](using F: => Or[F]): DerivedEmptyK[[x] =>> F[G[x]]] =
    new EmptyK[[x] =>> F[G[x]]]:
      lazy val f = F.unify
      def empty[A]: F[G[A]] = f.empty

  given nested[F[_], G[_]](using
      NotGiven[Or[F]]
  )(using F: DerivedPure.Or[F], G: => Or[G]): DerivedEmptyK[[x] =>> F[G[x]]] =
    new EmptyK[[x] =>> F[G[x]]]:
      val f = F.unify
      lazy val g = G.unify
      def empty[A]: F[G[A]] = f.pure(g.empty)

  given product[F[_]](using inst: K1.ProductInstances[Or, F]): DerivedEmptyK[F] = Strict.product(using inst.unify)
  inline given coproduct[F[_]](using K1.CoproductGeneric[F]): DerivedEmptyK[F] = Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Or[F]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[Or[F]]
  )(using DerivedPure.Or[F], Or[G]): DerivedEmptyK[[x] =>> F[G[x]]] =
    nested(using ev)

  object Strict:
    given product[F[_]](using inst: K1.ProductInstances[EmptyK, F]): DerivedEmptyK[F] = new EmptyK[F]:
      def empty[A]: F[A] = inst.construct([f[_]] => (F: EmptyK[f]) => F.empty[A])

    inline given coproduct[F[_]](using gen: K1.CoproductGeneric[F]): DerivedEmptyK[F] =
      gen.withOnly[Or, EmptyK[F]]([f[x] <: F[x]] => (F: Or[f]) => F.unify.asInstanceOf[EmptyK[F]])
