/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/traverse.scala
 *
 */
package cats.sequence

import shapeless._
import shapeless.ops.hlist._

sealed trait Traverser[L <: HList, P] extends Serializable {
  type Out
  def apply(hl: L): Out
}

object Traverser {
  type Aux[L <: HList, P, Out0] = Traverser[L, P] { type Out = Out0 }

  implicit def mkTraverser[L <: HList, P, S <: HList](
    implicit mapper: Mapper.Aux[P, L, S], sequencer: Sequencer[S]
  ): Aux[L, P, sequencer.Out] = new Traverser[L, P] {
    type Out = sequencer.Out
    def apply(hl: L): Out = sequencer(mapper(hl))
  }
}

trait TraverseFunctions {
  def traverse[L <: HList](hl: L)(f: Poly)(implicit traverser: Traverser[L, f.type]): traverser.Out =
    traverser(hl)
}

trait TraverseOps extends {
  implicit class withTraverse[L <: HList](self: L) {
    def traverse(f: Poly)(implicit traverser: Traverser[L, f.type]): traverser.Out =
      traverser(self)
  }
}
