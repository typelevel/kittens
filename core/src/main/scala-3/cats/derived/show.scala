package cats.derived

import cats.Show
import shapeless3.deriving.{Continue, K0, Labelling}

object show extends ShowDerivation

trait ShowDerivation:

  extension (F: Show.type)
    inline def derived[A](using gen: K0.Generic[A]): Show[A] = gen.derive(productShow, coproductShow)

  given productShow[A](using inst : => K0.ProductInstances[Show, A], l: Labelling[A]): Show[A] =
    (a: A) => {
      var idx = 0
      val x = inst.foldLeft[StringBuilder](a)(new StringBuilder(s"${l.label}("))(
        [t] => (acc: StringBuilder, sh: Show[t], t0: t) => {
          //elemLabels is backed by an array so this should be fast
          val res = Continue(acc.append(s"${l.elemLabels(idx)}=${sh.show(t0)}, "))
          idx = idx + 1
          res
          }
      )
      if(idx > 0) {x.delete(x.length - 2, x.length)}
      x.append(")")
      x.toString()
    }

  given coproductShow[A](using inst: => K0.CoproductInstances[Show, A]): Show[A] =
    (a: A) => inst.fold[String](a)(
      [t] => (sh: Show[t], t0: t) => sh.show(t0)
    )
