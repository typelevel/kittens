package cats.derived

import cats.{Bifunctor, Eval, Functor}
import shapeless3.deriving.Derived
import shapeless3.deriving.K2.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Bifunctor for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [a, b] =>> T
  * nested type [a, b] =>> G[H[a, b], H[a, b]] where G: Bifunctor and H: Bifunctor
  * generic case class where all fields form Bifunctor
  * generic sealed trait where all subclasses form Bifunctor
  * generic enum where all variants form Bifunctor""")
type DerivedBifunctor[F[_, _]] = Derived[Bifunctor[F]]
object DerivedBifunctor:
  inline def apply[F[_, _]]: Bifunctor[F] =
    import DerivedBifunctor.given
    summonInline[DerivedBifunctor[F]].instance

  inline def strict[F[_, _]]: Bifunctor[F] =
    import Strict.given
    summonInline[DerivedBifunctor[F]].instance

  given const[T]: DerivedBifunctor[Const[T]] = new Bifunctor[Const[T]]:
    def bimap[A, B, C, D](fab: T)(f: A => C, g: B => D): T = fab

  given leftId: DerivedBifunctor[Id1] = new Bifunctor[Id1]:
    override def bimap[A, B, C, D](fab: A)(f: A => C, g: B => D): C = f(fab)

  given rightId: DerivedBifunctor[Id2] = new Bifunctor[Id2]:
    override def bimap[A, B, C, D](fab: B)(f: A => C, g: B => D): D = g(fab)

  given left[F[_]](using F: Functor[F]): DerivedBifunctor[Left1[F]] = new Bifunctor[Left1[F]]:
    override def bimap[A, B, C, D](fab: F[A])(f: A => C, g: B => D): F[C] = F.map(fab)(f)

  given right[F[_]](using F: Functor[F]): DerivedBifunctor[Right1[F]] = new Bifunctor[Right1[F]]:
    override def bimap[A, B, C, D](fab: F[B])(f: A => C, g: B => D): F[D] = F.map(fab)(g)

  given nested[F[_, _], G[_, _]](using
      F: => (Bifunctor |: Derived)[F],
      G: => (Bifunctor |: Derived)[G]
  ): DerivedBifunctor[[a, b] =>> F[G[a, b], G[a, b]]] =
    new Lazy(() => F.unify.compose(using G.unify)) with Bifunctor[[a, b] =>> F[G[a, b], G[a, b]]]:
      export delegate.*

  given [F[_, _]](using inst: ProductInstances[Bifunctor |: Derived, F]): DerivedBifunctor[F] =
    Strict.product(using inst.unify)

  given [F[_, _]](using => CoproductInstances[Bifunctor |: Derived, F]): DerivedBifunctor[F] =
    Strict.coproduct

  private[derived] trait Safe[F[_, _]] extends Bifunctor[F]:
    private[derived] def safeBimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): Eval[F[C, D]]
    override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = safeBimap(fab)(f, g).value

  private[derived] def safeBimap[F[_, _], A, B, C, D](
      F: Bifunctor[F]
  )(fab: F[A, B])(f: A => C, g: B => D): Eval[F[C, D]] =
    F match
      case safe: Safe[F] @scala.unchecked => safe.safeBimap(fab)(f, g)
      case _ => Eval.later(F.bimap(fab)(f, g))

  trait Generic[T[f[_, _]] <: Bifunctor[f], F[_, _]](using inst: Instances[T, F]) extends Bifunctor[F]:
    final override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
      inst.map(fab)([f[_, _]] => (F: T[f], fa: f[A, B]) => F.bimap(fa)(f, g))

  trait Product[T[f[_, _]] <: Bifunctor[f], F[_, _]](using inst: ProductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeBimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): Eval[F[C, D]] =
      val pure = [a] => (x: a) => Eval.now(x)
      val mp = [a, b] => (ea: Eval[a], h: a => b) => ea.map(h)
      val ap = [a, b] => (ef: Eval[a => b], ea: Eval[a]) => ef.flatMap(h => ea.map(h))
      inst.traverse[A, B, Eval, C, D](fab)(mp)(pure)(ap)(
        [f[_, _]] => (F: T[f], fa: f[A, B]) => DerivedBifunctor.safeBimap(F)(fa)(f, g)
      )

  trait Coproduct[T[f[_, _]] <: Bifunctor[f], F[_, _]](using inst: CoproductInstances[T, F]) extends Safe[F]:
    private[derived] final override def safeBimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): Eval[F[C, D]] =
      Eval.defer(inst.fold(fab):
        [f[a, b] <: F[a, b]] => (F: T[f], fa: f[A, B]) =>
          DerivedBifunctor.safeBimap(F)(fa)(f, g).asInstanceOf[Eval[F[C, D]]]
      )

  object Strict:
    given product[F[_, _]: ProductInstancesOf[Bifunctor]]: DerivedBifunctor[F] =
      new Product[Bifunctor, F] {}

    given coproduct[F[_, _]](using inst: => CoproductInstances[Bifunctor |: Derived, F]): DerivedBifunctor[F] =
      given CoproductInstances[Bifunctor, F] = inst.unify
      new Coproduct[Bifunctor, F] {}
