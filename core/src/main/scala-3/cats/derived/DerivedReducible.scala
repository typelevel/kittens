package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.Derived
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Reducible for ${F}.
Make sure it satisfies one of the following conditions:
  * nested type [x] =>> G[H[x]] where G: Reducible and H: Reducible
  * generic case class where at least one field forms Reducible and the rest form Foldable
  * generic sealed trait where all subclasses form Reducible
  * generic enum where all variants form Reducible""")
type DerivedReducible[F[_]] = Derived[Reducible[F]]
object DerivedReducible:
  inline def apply[F[_]]: Reducible[F] =
    import DerivedFoldable.given
    import DerivedReducible.given
    summonInline[DerivedReducible[F]].instance

  inline def strict[F[_]]: Reducible[F] =
    import DerivedFoldable.Strict.given
    import DerivedReducible.Strict.given
    summonInline[DerivedReducible[F]].instance

  given nested[F[_], G[_]](using
      F: => (Reducible |: Derived)[F],
      G: => (Reducible |: Derived)[G]
  ): DerivedReducible[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with Reducible[F <<< G]:
      export delegate.*

  def product[F[_]](ev: Reducible[?])(using inst: ProductInstances[Foldable |: Derived, F]): DerivedReducible[F] =
    Strict.product(ev)(using inst.unify)

  inline given product[F[_]](using gen: ProductGeneric[F]): DerivedReducible[F] =
    product(summonFirst[Reducible |: Derived, gen.MirroredElemTypes].unify)

  given [F[_]](using => CoproductInstances[Reducible |: Derived, F]): DerivedReducible[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Reducible |: Derived, G[_]: Reducible |: Derived]: DerivedReducible[[x] =>> F[G[x]]] =
    nested

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
                case Some(b) => Some(F.foldLeft(fa, b)(g))
                case None => F.reduceLeftToOption(fa)(f)(g)
        .get

    final override def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst
        .foldRight[A, Eval[Option[B]]](fa)(evalNone):
          [f[_]] =>
            (F: T[f], fa: f[A], acc: Eval[Option[B]]) =>
              acc.flatMap:
                case Some(b) => F.foldRight(fa, Eval.now(b))(g).map(Some.apply)
                case None => F.reduceRightToOption(fa)(f)(g)
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

    given coproduct[F[_]](using inst: => CoproductInstances[Reducible |: Derived, F]): DerivedReducible[F] =
      given CoproductInstances[Reducible, F] = inst.unify
      new Coproduct[Reducible, F] {}
