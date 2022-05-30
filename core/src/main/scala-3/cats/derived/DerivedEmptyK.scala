package cats.derived

import alleycats.{Empty, EmptyK, Pure}
import cats.derived.util.Kinds
import shapeless3.deriving.{Const, K1}

import scala.annotation.implicitNotFound
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
  inline def apply[F[_]]: EmptyK[F] =
    import DerivedEmptyK.given
    summonInline[DerivedEmptyK[F]].instance

  given [T](using T: Empty[T]): DerivedEmptyK[Const[T]] =
    new EmptyK[Const[T]]:
      def empty[A] = T.empty

  given [F[_], G[_]](using F: Or[F]): DerivedEmptyK[[x] =>> F[G[x]]] =
    new EmptyK[[x] =>> F[G[x]]]:
      def empty[A] = F.unify.empty

  given [F[_], G[_]](using NotGiven[Or[F]])(using F: DerivedPure.Or[F], G: Or[G]): DerivedEmptyK[[x] =>> F[G[x]]] =
    new EmptyK[[x] =>> F[G[x]]]:
      def empty[A] = F.unify.pure(G.unify.empty)

  given product[F[_]](using inst: K1.ProductInstances[Or, F]): DerivedEmptyK[F] =
    new EmptyK[F]:
      def empty[A]: F[A] = inst.unify.construct([f[_]] => (E: EmptyK[f]) => E.empty[A])

  inline given coproduct[F[_]](using gen: K1.CoproductGeneric[F]): DerivedEmptyK[F] =
    Kinds.summonOne1[Or, gen.MirroredElemTypes, F].unify
