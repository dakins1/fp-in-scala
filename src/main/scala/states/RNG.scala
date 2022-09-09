package states

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed =
      (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `states.RNG` instance created from the new seed.
    val n =
      (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `states.RNG` state.
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // allows you to map the output of the rng without affecting the state of the rng
  // okay i see now, this is still a function composition, and we've added on an additional layer
  // of always composing f onto the output. So Rand[B] is really a Rand[A] that always gets f applied
  // to it right afterwards. states.RNG remains the same type all the way through -- e.g. not states.RNG[B]
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def elegantDouble(rng: RNG): (Double, RNG) = {
    val f = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
    f(rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) (Nil, rng)
    else {
      val (i, rng2) = rng.nextInt
      val (ls, rngx) = ints(count - 1)(rng2)
      (i :: ls, rngx)
    }
  }

  def sequence[A](transitions: Seq[Rand[A]]): Rand[List[A]] = {
    transitions.foldLeft[Rand[List[A]]](unit(Nil)) { (acc, tx) =>
      map2(acc, tx) { (ls, a) => a :: ls }
    }
  }

  def intsSeq(count: Int)(rng: RNG): (List[Int], RNG) = {
    val ls = List.fill[Rand[Int]](count)(_.nextInt)
    sequence(ls)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng1) = f(rng)
    g(a)(rng1) //g(a) returns a Rand[B]: RNG => (B, RNG) - then we pass in rng1 to get (B, rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { x =>
      val mod = x % n
      if (x + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapf[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2f[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(f(a, _)))

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  type Rand[+A] = RNG => (A, RNG)
}
