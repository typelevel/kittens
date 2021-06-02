package cats.derived

import scala.compiletime._

final case class OrElse[+A, +B](unify: A | B)
object OrElse:
  inline given [A, B]: (A OrElse B) = summonFrom {
    case a: A => OrElse(a)
    case b: B => OrElse(b)
  }

final case class Alt1[+A[_[_]], +B[_[_]], F[_]](unify: A[F] | B[F])
object Alt1:
  inline given [A[_[_]], B[_[_]], F[_]]: Alt1[A, B, F] = summonFrom {
    case af: A[F] => Alt1(af)
    case bf: B[F] => Alt1(bf)
  }
