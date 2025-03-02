package cats.derived

import cats.{Applicative, Bitraverse, Eval, Traverse}
import shapeless3.deriving.K2.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Bitraverse for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [a, b] =>> T
  * nested type [a, b] =>> G[H[a, b], H[a, b]] where G: Bitraverse and H: Bitraverse
  * generic case class where all fields form Bitraverse
  * generic sealed trait where all subclasses form Bitraverse
  * generic enum where all variants form Bitraverse""")
type DerivedBitraverse[F[_, _]] = Derived[Bitraverse[F]]
object DerivedBitraverse:
  @nowarn("msg=unused import")
  inline def apply[F[_, _]]: Bitraverse[F] =
    import DerivedBitraverse.given
    summonInline[DerivedBitraverse[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_, _]]: Bitraverse[F] =
    import Strict.given
    summonInline[DerivedBitraverse[F]].instance

  given const[T]: DerivedBitraverse[[_, _] =>> T] = new Bitraverse[[_, _] =>> T]:
    override def bimap[A, B, C, D](fab: T)(f: A => C, g: B => D): T = fab
    override def bitraverse[G[_], A, B, C, D](fab: T)(f: A => G[C], g: B => G[D])(using G: Applicative[G]): G[T] =
      G.pure(fab)

    override def bifoldLeft[A, B, C](fab: T, c: C)(f: (C, A) => C, g: (C, B) => C): C = c
    override def bifoldRight[A, B, C](fab: T, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = c

  given leftId: DerivedBitraverse[[a, _] =>> a] = new Bitraverse[[a, _] =>> a]:
    override def bimap[A, B, C, D](fab: A)(f: A => C, g: B => D): C = f(fab)
    override def bitraverse[G[_]: Applicative, A, B, C, D](fab: A)(f: A => G[C], g: B => G[D]): G[C] = f(fab)
    override def bifoldLeft[A, B, C](fab: A, c: C)(f: (C, A) => C, g: (C, B) => C): C = f(c, fab)
    override def bifoldRight[A, B, C](fab: A, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = f(fab, c)

  given rightId: DerivedBitraverse[[_, b] =>> b] = new Bitraverse[[_, b] =>> b]:
    override def bimap[A, B, C, D](fab: B)(f: A => C, g: B => D): D = g(fab)
    override def bitraverse[G[_]: Applicative, A, B, C, D](fab: B)(f: A => G[C], g: B => G[D]): G[D] = g(fab)
    override def bifoldLeft[A, B, C](fab: B, c: C)(f: (C, A) => C, g: (C, B) => C): C = g(c, fab)
    override def bifoldRight[A, B, C](fab: B, c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = g(fab, c)

  given left[F[_]](using F: Traverse[F]): DerivedBitraverse[[a, _] =>> F[a]] = new Bitraverse[[a, _] =>> F[a]]:
    override def bimap[A, B, C, D](fab: F[A])(f: A => C, g: B => D): F[C] = F.map(fab)(f)
    override def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[A])(f: A => G[C], g: B => G[D]): G[F[C]] =
      F.traverse(fab)(f)
    override def bifoldLeft[A, B, C](fab: F[A], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      F.foldLeft(fab, c)(f)
    override def bifoldRight[A, B, C](fab: F[A], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = F.foldRight(fab, c)(f)

  given right[F[_]](using F: Traverse[F]): DerivedBitraverse[[_, b] =>> F[b]] = new Bitraverse[[_, b] =>> F[b]]:
    override def bimap[A, B, C, D](fab: F[B])(f: A => C, g: B => D): F[D] = F.map(fab)(g)
    override def bitraverse[G[_]: Applicative, A, B, C, D](fab: F[B])(f: A => G[C], g: B => G[D]): G[F[D]] =
      F.traverse(fab)(g)
    override def bifoldLeft[A, B, C](fab: F[B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      F.foldLeft(fab, c)(g)
    override def bifoldRight[A, B, C](fab: F[B], c: Eval[C])(
        f: (A, Eval[C]) => Eval[C],
        g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = F.foldRight(fab, c)(g)

  given nested[F[_, _], G[_, _]](using
      F: => Derived.Or[Bitraverse[F]],
      G: => Derived.Or[Bitraverse[G]]
  ): DerivedBitraverse[[a, b] =>> F[G[a, b], G[a, b]]] =
    new Derived.Lazy(() => F.compose(using G)) with Bitraverse[[a, b] =>> F[G[a, b], G[a, b]]]:
      export delegate.*

  given [F[_, _]: ProductInstancesOf[Derived.Or2[Bitraverse]]]: DerivedBitraverse[F] =
    Strict.product

  given [F[_, _]](using => CoproductInstances[Derived.Or2[Bitraverse], F]): DerivedBitraverse[F] =
    Strict.coproduct

  trait Product[T[f[_, _]] <: Bitraverse[f], F[_, _]](using inst: ProductInstances[T, F])
      extends Bitraverse[F]
      with DerivedBifunctor.Generic[T, F]
      with DerivedBifoldable.Product[T, F]:

    final override def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(using
        G: Applicative[G]
    ): G[F[C, D]] =
      val pure = [a] => (x: a) => G.pure(x)
      val map = [a, b] => (ga: G[a], f: a => b) => G.map(ga)(f)
      val ap = [a, b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga)
      inst.traverse[A, B, G, C, D](fab)(map)(pure)(ap)([f[_, _]] => (F: T[f], fab: f[A, B]) => F.bitraverse(fab)(f, g))

  trait Coproduct[T[f[_, _]] <: Bitraverse[f], F[_, _]](using inst: CoproductInstances[T, F])
      extends Bitraverse[F]
      with DerivedBifunctor.Generic[T, F]
      with DerivedBifoldable.Coproduct[T, F]:

    final override def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(using
        G: Applicative[G]
    ): G[F[C, D]] = inst.fold(fab):
      [f[a, b] <: F[a, b]] => (F: T[f], fa: f[A, B]) => G.widen[f[C, D], F[C, D]](F.bitraverse(fa)(f, g))

  object Strict:
    given product[F[_, _]: ProductInstancesOf[Bitraverse]]: DerivedBitraverse[F] =
      new Bitraverse[F] with Product[Bitraverse, F] {}

    given coproduct[F[_, _]](using inst: => CoproductInstances[Derived.Or2[Bitraverse], F]): DerivedBitraverse[F] =
      new Bitraverse[F] with Coproduct[Bitraverse, F] {}
