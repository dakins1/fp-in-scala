// import fpinscala.datastructures._
import fpinscala.mystuff._

object Main extends App {
  
    val x = Cons(1, Cons(2,Nil))
    val y = MyList(1,2,3,4)
    println(x.length)
    println(y.length)
    println(y.foldRight(0)(_+_))
    println(y.foldRight(1)(_*_))

}
