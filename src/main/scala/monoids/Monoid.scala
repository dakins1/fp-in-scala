package monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def identity: A
}

object MonoidApp extends App {

  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String) = s1 + s2
    val identity = ""
  }

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val identity = 0
  }

  val intMult = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val identity = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y
    val identity = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val identity = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x.orElse(y)
    val identity = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f.compose(g)
    val identity = a => a
  }

  // fold for a monoid is basically a concatenate
  // when A doesn't have a Monoid instance, but we can map it into a type B that does and then fold
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.identity)((b, a) => m.op(b, f(a)))
  }

  def foldRightViaFoldMap[A, B](z: B, lst: List[A])(f: (A, B) => B): B = {
    foldMap(lst, endoMonoid[B])(f.curried)(z)
  }

  // balanced fold - going down middle instead of right/left associativity
  // split in the middle, recursively process each half, then join back together with the monoid op
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.identity
    else if (v.length == 1) f(v.head)
    else {
      val (a1, a2) = v.splitAt(v.length / 2)
      m.op(foldMapV(a1, m)(f), foldMapV(a2, m)(f))
    }
  }

  // use foldMap to determine if an indexedSeq is ordered
//  def isOrdered(ints: IndexedSeq[Int]): Boolean = {
//    // make a monoid for determining order...i think
//    type Comp = Int => (Int, Boolean)
//    val ordered = new Monoid[Comp] {
//      def op(x: Comp, y: Comp): Comp = {
//        identity.compose(x)
//      }
//      val identity: Comp = i => (i, true)
//    }
//  }

}
