package cats.derived

import cats.derived.Derived.<<<
import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.Continue
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Reducible[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type [x] =>> G[H[x]] where G: Reducible and H: Reducible
  * it is a generic case class where at least one field has a Reducible and the rest Foldable instances
  * it is a generic sealed trait where all subclasses have a Reducible instance""")
type DerivedReducible[F[_]] = Derived[Reducible[F]]
object DerivedReducible:
  type Or[F[_]] = Derived.Or[Reducible[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Reducible[F] =
    import DerivedFoldable.given
    import DerivedReducible.given
    summonInline[DerivedReducible[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Reducible[F] =
    import DerivedFoldable.Strict.given
    import DerivedReducible.Strict.given
    summonInline[DerivedReducible[F]].instance

  given nested[F[_], G[_]](using
      F: => DerivedReducible.Or[F],
      G: => DerivedReducible.Or[G]
  ): DerivedReducible[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Reducible[F <<< G]:
      export delegate.*

  def product[F[_]: ProductInstancesOf[DerivedFoldable.Or]](ev: Reducible[?]): DerivedReducible[F] =
    Strict.product(ev)(using ProductInstances.unify)

  inline given product[F[_]](using gen: ProductGeneric[F]): DerivedReducible[F] =
    product(summonFirst[DerivedReducible.Or, gen.MirroredElemTypes].unify)

  given [F[_]](using => CoproductInstances[Or, F]): DerivedReducible[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: DerivedReducible.Or, G[_]: DerivedReducible.Or]: DerivedReducible[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Foldable[f], F[_]](@unused ev: Reducible[?])(using inst: ProductInstances[T, F])
      extends DerivedFoldable.Product[T, F],
        Reducible[F]:

    private val evalNone = Eval.now(None)

    final override def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst
        .foldLeft[A, Option[B]](fa)(None):
          [f[_]] =>
            (acc: Option[B], F: T[f], fa: f[A]) =>
              acc match
                case Some(b) => Continue(Some(F.foldLeft(fa, b)(g)))
                case None => Continue(F.reduceLeftToOption(fa)(f)(g))
        .get

    final override def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst
        .foldRight[A, Eval[Option[B]]](fa)(evalNone):
          [f[_]] =>
            (F: T[f], fa: f[A], acc: Eval[Option[B]]) =>
              Continue(acc.flatMap {
                case Some(b) => F.foldRight(fa, Eval.now(b))(g).map(Some.apply)
                case None => F.reduceRightToOption(fa)(f)(g)
              })
        .map(_.get)

  trait Coproduct[T[f[_]] <: Reducible[f], F[_]](using inst: CoproductInstances[T, F])
      extends DerivedFoldable.Coproduct[T, F],
        Reducible[F]:

    final override def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.reduceLeftTo(fa)(f)(g))

    final override def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.reduceRightTo(fa)(f)(g)))

  object Strict:
    def product[F[_]: ProductInstancesOf[Foldable]](ev: Reducible[?]): DerivedReducible[F] =
      new Product[Foldable, F](ev) {}

    inline given product[F[_]](using gen: ProductGeneric[F]): DerivedReducible[F] =
      product(summonFirst[Reducible, gen.MirroredElemTypes])

    given coproduct[F[_]](using inst: => CoproductInstances[Or, F]): DerivedReducible[F] =
      given CoproductInstances[Reducible, F] = inst.unify
      new Coproduct[Reducible, F] {}
