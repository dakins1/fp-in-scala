
object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  import Prop._
  def check:Either[(FailedCase, SuccessCount), SuccessCount]
//  def &&(p:Prop) = this.check && p.check
}


case class State[S,A](run: S => (A,S))
sealed trait RNG
case class Gen[A](sample: State[RNG,A])


object Gen {

  // generate intergers in the range start to stop
  // in this case, we return a Gen[Int] which WILL (not now, but WILL) generate these integers
  def choose(start:Int, stopExclusive:Int):Gen[Int] = ???

  def listOf[A](a:Gen[A]):Gen[List[A]] = ???
  def forAll[A](a:Gen[A])(f: A => Boolean): Prop = ???


}
