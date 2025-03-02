package cats.derived

import cats.{Bifunctor, Functor}
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
  @nowarn("msg=unused import")
  inline def apply[F[_, _]]: Bifunctor[F] =
    import DerivedBifunctor.given
    summonInline[DerivedBifunctor[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_, _]]: Bifunctor[F] =
    import Strict.given
    summonInline[DerivedBifunctor[F]].instance

  given const[T]: DerivedBifunctor[[_, _] =>> T] = new Bifunctor[[_, _] =>> T]:
    def bimap[A, B, C, D](fab: T)(f: A => C, g: B => D): T = fab

  given leftId: DerivedBifunctor[[a, _] =>> a] = new Bifunctor[[a, _] =>> a]:
    override def bimap[A, B, C, D](fab: A)(f: A => C, g: B => D): C = f(fab)

  given rightId: DerivedBifunctor[[_, b] =>> b] = new Bifunctor[[_, b] =>> b]:
    override def bimap[A, B, C, D](fab: B)(f: A => C, g: B => D): D = g(fab)

  given left[F[_]](using F: Functor[F]): DerivedBifunctor[[a, _] =>> F[a]] = new Bifunctor[[a, _] =>> F[a]]:
    override def bimap[A, B, C, D](fab: F[A])(f: A => C, g: B => D): F[C] = F.map(fab)(f)

  given right[F[_]](using F: Functor[F]): DerivedBifunctor[[_, b] =>> F[b]] = new Bifunctor[[_, b] =>> F[b]]:
    override def bimap[A, B, C, D](fab: F[B])(f: A => C, g: B => D): F[D] = F.map(fab)(g)

  given nested[F[_, _], G[_, _]](using
      F: => Derived.Or[Bifunctor[F]],
      G: => Derived.Or[Bifunctor[G]]
  ): DerivedBifunctor[[a, b] =>> F[G[a, b], G[a, b]]] =
    new Derived.Lazy(() => F.compose(using G)) with Bifunctor[[a, b] =>> F[G[a, b], G[a, b]]]:
      export delegate.*

  given generic[F[_, _]](using inst: => Instances[Derived.Or2[Bifunctor], F]): DerivedBifunctor[F] = gen
  private def gen[F[_, _]: InstancesOf[Bifunctor]]: DerivedBifunctor[F] = new Generic[Bifunctor, F] {}

  trait Generic[T[f[_, _]] <: Bifunctor[f], F[_, _]](using inst: Instances[T, F]) extends Bifunctor[F]:
    final override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
      inst.map(fab)([f[_, _]] => (F: T[f], fa: f[A, B]) => F.bimap(fa)(f, g))

  object Strict:
    given product[F[_, _]: ProductInstancesOf[Bifunctor]]: DerivedBifunctor[F] = gen
    given coproduct[F[_, _]](using inst: => CoproductInstances[Derived.Or2[Bifunctor], F]): DerivedBifunctor[F] = gen
