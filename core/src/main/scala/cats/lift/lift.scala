/**
  * Originally adapted from shapeless-contrib
  * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/lift.scala
  */
package cats.lift

import cats.{Applicative, Apply}
import cats.implicits._

import shapeless._
import shapeless.ops.function._
import shapeless.syntax.std.function._

trait LifterAux[G[_], I <: HList, R, GI <: HList] {
  def apply(gf: G[I => R])(implicit G: Apply[G]): GI => G[R]
}

object LifterAux {

  implicit def liftZero[G[_], R]: LifterAux[G, HNil, R, HNil] = new LifterAux[G, HNil, R, HNil] {
    def apply(gf: G[HNil => R])(implicit G: Apply[G]) = _ =>
      gf map { _(HNil) }
  }

  implicit def liftCons[G[_], H, T <: HList, R, GI <: HList](implicit tail: LifterAux[G, T, R, GI]): LifterAux[G, H :: T, R, G[H] :: GI] = new LifterAux[G, H :: T, R, G[H] :: GI] {
    def apply(gf: G[(H :: T) => R])(implicit G: Apply[G]) = {
      case gh :: gi =>
        tail(G.map2(gh, gf) { (h, f) => t => f(h :: t) })(G)(gi)
    }
  }
}

trait Lifts {

  implicit class ApplicativeOps[G[_]](instance: Applicative[G]) {
    def liftA[F, R, I <: HList, GI <: HList, OF](f: F)(
      implicit hlister: FnToProduct.Aux[F, I => R],
      lifter: LifterAux[G, I, R, GI],
      unhlister: FnFromProduct.Aux[GI => G[R], OF]
    ): OF =
      lifter(instance.pure(f.toProduct))(instance).fromProduct
  }

}


object Lifts extends Lifts
