package states.diff

import states.RNG

class State[S, +A] {
  type State[S, +A] = S => (A, S)

  type Rand[A] = State[RNG, A]

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = { st =>
    val (a, st1) = f(st)
    g(a)(st1)
  }

  def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = ???
}
