package cats.derived

import scala.compiletime._

enum OrElse[+A, +B]:
  case Primary(value: A)
  case Secondary(value: () => B)

  final def fold[C](primary: A => C, secondary: B => C): C = this match
    case Primary(value) => primary(value)
    case Secondary(value) => secondary(value())

  final def unify[C >: A](implicit ev: B <:< C): C = this match
    case Primary(value) => value
    case Secondary(value) => value()

object OrElse:
  inline given [A, B]: OrElse[A, B] = summonFrom {
    case a: A => Primary(a)
    case b: B => Secondary(() => b)
  }
