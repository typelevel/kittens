package cats.derived

import cats.Show
import shapeless3.deriving.{Continue, K0, Labelling}

trait ShowDerivation:

  extension (F: Show.type)
    inline def derived[A](using gen: K0.Generic[A]): Show[A] =
      gen.derive(product, coproduct)

  given product[A](using inst: => K0.ProductInstances[Show, A], labelling: Labelling[A]): Show[A] = a =>
    val prefix = labelling.label
    val labels = labelling.elemLabels
    val n = labels.size
    if n <= 0 then prefix else
      val sb = new StringBuilder(prefix)
      sb.append('(')
      var i = 0
      while i < n do
        sb.append(labels(i))
        sb.append('=')
        sb.append(inst.project(a)(i)([t] => (show: Show[t], x: t) => show.show(x)))
        sb.append(", ")
        i += 1

      val l = sb.length
      sb.delete(l - 2, l)
      sb.append(')')
      sb.toString

  given coproduct[A](using inst: => K0.CoproductInstances[Show, A]): Show[A] =
    inst.fold(_)([t] => (show: Show[t], x: t) => show.show(x))
