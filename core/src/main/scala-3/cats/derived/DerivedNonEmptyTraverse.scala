package cats.derived

import cats.{Applicative, Apply, Eval, NonEmptyTraverse, Traverse}
import shapeless3.deriving.Derived
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive NonEmptyTraverse for ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * nested type [x] =>> G[H[x]] where G: NonEmptyTraverse and H: NonEmptyTraverse
  * generic case class where at least one field forms NonEmptyTraverse and the rest form Traverse
  * generic sealed trait where all subclasses form NonEmptyTraverse
  * generic enum where all variants form NonEmptyTraverse""")
type DerivedNonEmptyTraverse[F[_]] = Derived[NonEmptyTraverse[F]]
object DerivedNonEmptyTraverse:
  inline def apply[F[_]]: NonEmptyTraverse[F] =
    import DerivedTraverse.given
    import DerivedNonEmptyTraverse.given
    summonInline[DerivedNonEmptyTraverse[F]].instance

  inline def strict[F[_]]: NonEmptyTraverse[F] =
    import DerivedTraverse.Strict.given
    import DerivedNonEmptyTraverse.Strict.given
    summonInline[DerivedNonEmptyTraverse[F]].instance

  /** Stack-safe (trampolined via [[cats.Eval]]) derivation. Opt-in: slower on shallow data, but does not overflow the
    * stack on deeply nested recursive ADTs.
    */
  inline def stackSafe[F[_]]: NonEmptyTraverse[F] =
    import DerivedTraverse.StackSafe.given
    import DerivedNonEmptyTraverse.StackSafe.given
    summonInline[DerivedNonEmptyTraverse[F]].instance

  given nested[F[_], G[_]](using
      F: => (NonEmptyTraverse |: Derived)[F],
      G: => (NonEmptyTraverse |: Derived)[G]
  ): DerivedNonEmptyTraverse[F <<< G] =
    new Lazy(() => F.unify.compose(using G.unify)) with NonEmptyTraverse[F <<< G]:
      export delegate.*

  def product[F[_]](ev: NonEmptyTraverse[?])(using
      inst: ProductInstances[Traverse |: Derived, F]
  ): DerivedNonEmptyTraverse[F] =
    Strict.product(ev)(using inst.unify)

  inline given product[F[_]](using gen: ProductGeneric[F]): DerivedNonEmptyTraverse[F] =
    product(summonFirst[NonEmptyTraverse |: Derived, gen.MirroredElemTypes].unify)

  given [F[_]](using => CoproductInstances[NonEmptyTraverse |: Derived, F]): DerivedNonEmptyTraverse[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: NonEmptyTraverse |: Derived, G[_]: NonEmptyTraverse |: Derived]
      : DerivedNonEmptyTraverse[[x] =>> F[G[x]]] = nested

  private type Alt[F[_]] = [A] =>> Either[F[A], A]
  private def altApplicative[F[_]](using F: Apply[F]): Applicative[Alt[F]] = new Applicative[Alt[F]]:
    override def pure[A](x: A) = Right(x)
    override def map[A, B](fa: Alt[F][A])(f: A => B) = fa match
      case Left(fa) => Left(F.map(fa)(f))
      case Right(a) => Right(f(a))
    override def ap[A, B](ff: Alt[F][A => B])(fa: Alt[F][A]) = (ff, fa) match
      case (Left(ff), Left(fa)) => Left(F.ap(ff)(fa))
      case (Left(ff), Right(a)) => Left(F.map(ff)(_(a)))
      case (Right(f), Left(fa)) => Left(F.map(fa)(f))
      case (Right(f), Right(a)) => Right(f(a))

  private given [F[_]](using F: Apply[F]): Applicative[Alt[F]] = altApplicative[F]

  // ---- Default: fast direct recursion ----

  trait Product[T[x[_]] <: Traverse[x], F[_]](@unused ev: NonEmptyTraverse[?])(using
      @unused inst: ProductInstances[T, F]
  ) extends NonEmptyTraverse[F],
        DerivedReducible.Product[T, F],
        DerivedTraverse.Product[T, F]:

    final override def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      traverse[Alt[G], A, B](fa)(f.andThen(Left.apply)) match
        case Left(value) => value
        case Right(_) => ???

  trait Coproduct[T[x[_]] <: NonEmptyTraverse[x], F[_]](using inst: CoproductInstances[T, F])
      extends NonEmptyTraverse[F],
        DerivedReducible.Coproduct[T, F],
        DerivedTraverse.Coproduct[T, F]:

    final override def nonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Apply[G]): G[F[B]] =
      inst.fold(fa)([f[a] <: F[a]] => (F: T[f], fa: f[A]) => G.widen[f[B], F[B]](F.nonEmptyTraverse(fa)(f)))

  object Strict:
    def product[F[_]: ProductInstancesOf[Traverse]](ev: NonEmptyTraverse[?]): DerivedNonEmptyTraverse[F] =
      new Product[Traverse, F](ev)
        with DerivedReducible.Product[Traverse, F](ev)
        with DerivedTraverse.Product[Traverse, F]
        with DerivedFunctor.Generic[Traverse, F] {}

    inline given product[F[_]](using gen: ProductGeneric[F]): DerivedNonEmptyTraverse[F] =
      product(summonFirst[NonEmptyTraverse, gen.MirroredElemTypes])

    given coproduct[F[_]](using
        inst: => CoproductInstances[NonEmptyTraverse |: Derived, F]
    ): DerivedNonEmptyTraverse[F] =
      given CoproductInstances[NonEmptyTraverse, F] = inst.unify
      new NonEmptyTraverse[F] with Coproduct[NonEmptyTraverse, F] {}

  // ---- Opt-in: stack-safe recursion via Eval ----

  object StackSafe:
    def product[F[_]: ProductInstancesOf[Traverse]](ev: NonEmptyTraverse[?]): DerivedNonEmptyTraverse[F] =
      new SafeProduct[Traverse, F](ev) with DerivedReducible.SafeProduct[Traverse, F](ev) {}

    inline given product[F[_]](using gen: ProductGeneric[F]): DerivedNonEmptyTraverse[F] =
      product(summonFirst[NonEmptyTraverse, gen.MirroredElemTypes])

    given coproduct[F[_]](using
        inst: => CoproductInstances[NonEmptyTraverse |: Derived, F]
    ): DerivedNonEmptyTraverse[F] =
      given CoproductInstances[NonEmptyTraverse, F] = inst.unify
      new SafeCoproduct[NonEmptyTraverse, F] {}

  trait SafeProduct[T[x[_]] <: Traverse[x], F[_]](@unused ev: NonEmptyTraverse[?])(using
      @unused inst: ProductInstances[T, F]
  ) extends NonEmptyTraverse[F],
        DerivedReducible.SafeProduct[T, F],
        DerivedTraverse.SafeProduct[T, F]:

    final override def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      given Applicative[Alt[G]] = altApplicative[G]
      DerivedTraverse.safeTraverse[F, Alt[G], A, B](this)(fa)(f.andThen(Left.apply)).value match
        case Left(value) => value
        case Right(_) => ???

  trait SafeCoproduct[T[x[_]] <: NonEmptyTraverse[x], F[_]](using inst: CoproductInstances[T, F])
      extends NonEmptyTraverse[F],
        DerivedReducible.SafeCoproduct[T, F],
        DerivedTraverse.SafeCoproduct[T, F]:

    final override def nonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Apply[G]): G[F[B]] =
      safeNonEmptyTraverse(fa)(f).value

    private[derived] def safeNonEmptyTraverse[G[_], A, B](
        fa: F[A]
    )(f: A => G[B])(using G: Apply[G]): Eval[G[F[B]]] =
      Eval.defer(inst.fold(fa):
        [f[a] <: F[a]] => (F: T[f], fa: f[A]) =>
          Eval.later(F.nonEmptyTraverse(fa)(f)).map(g => G.widen[f[B], F[B]](g)).asInstanceOf[Eval[G[F[B]]]]
      )
