package states

import states.RNG

object RNGApp extends App {
  val rng = Simple(Long.MaxValue)
  println(rng.intsSeq(10)(rng))
}
