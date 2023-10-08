package my.playground
package fpinscala.exceptions

// Make Validated not depend on List[E], so we need a combining action to combine errors
enum Validated2[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)

  def map2[EE >: E, B, C](b: Validated2[EE, B])(f: (A, B) => C)(combineErrors: (EE, EE) => EE): Validated2[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb))     => Valid(f(aa, bb))
      case (Valid(_), Invalid(e))     => Invalid(e)
      case (Invalid(e), Valid(_))     => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(combineErrors(e1, e2))

  def traverse[E, A, B](as: List[A], f: A => Validated2[E, B], combineErrors: (E, E) => E): Validated2[E, List[B]] =
    as.foldRight[Validated2[E, List[B]]](Valid(Nil))((a, acc) => f(a).map2(acc)(_ :: _)(combineErrors))

  def sequence[E, A](vs: List[Validated2[E, A]], combineErrors: (E, E) => E): Validated2[E, List[A]] =
    traverse(vs, identity, combineErrors)
