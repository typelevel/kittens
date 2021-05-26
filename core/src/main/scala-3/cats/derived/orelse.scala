package cats.derived

import scala.compiletime._

sealed trait OrElse[+A, +B]:
  def unify: A | B

object OrElse:
  final class Primary[+A](val unify: A) extends OrElse[A, Nothing]
  final class Secondary[+B](value: => B) extends OrElse[Nothing, B]:
    lazy val unify: B = value

  inline given [A, B]: OrElse[A, B] = summonFrom {
    case a: A => Primary(a)
    case b: B => Secondary(b)
  }
