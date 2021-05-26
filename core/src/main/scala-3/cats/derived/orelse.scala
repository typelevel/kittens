package cats.derived

enum OrElse[+A, +B]:
  case Primary(value: A)
  case Secondary(value: () => B)

  final def fold[C](primary: A => C, secondary: B => C): C = this match
    case Primary(value) => primary(value)
    case Secondary(value) => secondary(value())

  final def unify[C >: A](implicit ev: B <:< C): C = this match
    case Primary(value) => value
    case Secondary(value) => value()

object OrElse extends OrElseLowPriority:
  inline given primary[A, B](using inline a: A): OrElse[A, B] =
    OrElse.Primary(a)

private[derived] sealed abstract class OrElseLowPriority:
  inline given secondary[A, B](using inline b: B): OrElse[A, B] =
    OrElse.Secondary(() => b)
