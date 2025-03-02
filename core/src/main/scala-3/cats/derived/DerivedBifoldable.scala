package cats.derived

import cats.{Bifoldable, Eval, Foldable}
import shapeless3.deriving.Continue
import shapeless3.deriving.K2.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Bifoldable for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [a, b] =>> T
  * nested type [a, b] =>> G[H[a, b], H[a, b]] where G: Bifoldable and H: Bifoldable
  * generic case class where all fields form Bifoldable
  * generic sealed trait where all subclasses form Bifoldable
  * generic enum where all variants form Bifoldable""")
type DerivedBifoldable[F[_, _]] = Derived[Bifoldable[F]]
object DerivedBifoldable:
  @nowarn("msg=unused import")
  inline def apply[F[_, _]]: Bifoldable[F] =
    import DerivedBifoldable.given
    summonInline[DerivedBifoldable[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_, _]]: Bifoldable[F] =
    import Strict.given
    summonInline[DerivedBifoldable[F]].instance

  given const[T]: DerivedBifoldable[Const[T]] = new Bifoldable[Const[T]]:
    override def bifoldLeft[A, B, C](fab: T, c: C)(f: (C, A) => C, g: (C, B) => C): C = c
    override def bifoldRight[A, B, C](fab: T, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = c

  given leftId: DerivedBifoldable[Id1] = new Bifoldable[Id1]:
    override def bifoldLeft[A, B, C](fab: A, c: C)(f: (C, A) => C, g: (C, B) => C): C = f(c, fab)
    override def bifoldRight[A, B, C](fab: A, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = f(fab, c)

  given rightId: DerivedBifoldable[Id2] = new Bifoldable[Id2]:
    override def bifoldLeft[A, B, C](fab: B, c: C)(f: (C, A) => C, g: (C, B) => C): C = g(c, fab)
    override def bifoldRight[A, B, C](fab: B, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = g(fab, c)

  given left[F[_]](using F: Foldable[F]): DerivedBifoldable[Left1[F]] = new Bifoldable[Left1[F]]:
    override def bifoldLeft[A, B, C](fab: F[A], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      F.foldLeft(fab, c)(f)
    override def bifoldRight[A, B, C](fab: F[A], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = F.foldRight(fab, c)(f)

  given right[F[_]](using F: Foldable[F]): DerivedBifoldable[Right1[F]] = new Bifoldable[Right1[F]]:
    override def bifoldLeft[A, B, C](fab: F[B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      F.foldLeft(fab, c)(g)
    override def bifoldRight[A, B, C](fab: F[B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = F.foldRight(fab, c)(g)

  given nested[F[_, _], G[_, _]](using
      F: => Derived.Or[Bifoldable[F]],
      G: => Derived.Or[Bifoldable[G]]
  ): DerivedBifoldable[[a, b] =>> F[G[a, b], G[a, b]]] =
    new Derived.Lazy(() => F.compose(using G)) with Bifoldable[[a, b] =>> F[G[a, b], G[a, b]]]:
      export delegate.*

  given [F[_, _]: ProductInstancesOf[Derived.Or2[Bifoldable]]]: DerivedBifoldable[F] =
    Strict.product

  given [F[_, _]](using => CoproductInstances[Derived.Or2[Bifoldable], F]): DerivedBifoldable[F] =
    Strict.coproduct

  trait Product[T[f[_, _]] <: Bifoldable[f], F[_, _]](using inst: ProductInstances[T, F]) extends Bifoldable[F]:
    final override def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      inst.foldLeft(fab)(c)([f[_, _]] => (c: C, F: T[f], fab: f[A, B]) => Continue(F.bifoldLeft(fab, c)(f, g)))

    final override def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = inst.foldRight(fab)(c):
      [f[_, _]] => (F: T[f], fab: f[A, B], c: Eval[C]) => Continue(Eval.defer(F.bifoldRight(fab, c)(f, g)))

  trait Coproduct[T[f[_, _]] <: Bifoldable[f], F[_, _]](using inst: CoproductInstances[T, F]) extends Bifoldable[F]:
    final override def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      inst.fold(fab)([f[_, _]] => (F: T[f], fab: f[A, B]) => F.bifoldLeft(fab, c)(f, g))

    final override def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = inst.fold(fab):
      [f[_, _]] => (F: T[f], fab: f[A, B]) => Eval.defer(F.bifoldRight(fab, c)(f, g))

  object Strict:
    given product[F[_, _]: ProductInstancesOf[Bifoldable]]: DerivedBifoldable[F] =
      new Product[Bifoldable, F] {}

    given coproduct[F[_, _]](using inst: => CoproductInstances[Derived.Or2[Bifoldable], F]): DerivedBifoldable[F] =
      new Coproduct[Bifoldable, F] {}
