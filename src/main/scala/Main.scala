// import fpinscala.datastructures._
import fpinscala.mystuff._

object Main extends App {
    val x = Cons(1, Cons(2,Nil))
    val y = MyList(1,2,3,4)
    println(x.length())
    println(y.length())
    println(x.foldRight(0)(_+_))
    println(y.foldLeft(1)(_*_))

    println(y.foldRight(Nil:MyList[Int])(Cons(_,_)))
    println(y.foldLeft(Nil:MyList[Int])(Cons(_,_)))

    println(y.reverse)

    println(y.foldRight(Nil:MyList[Int])(Cons(_,_)))
    println(y.foldRight_nonTailRec(Nil:MyList[Int])(Cons(_,_)))


    println(y.foldLeftViaFoldRight(Nil:MyList[Int])(Cons(_,_)))

    println(y.append(MyList(7)))

    val lsts = MyList(
        MyList(1,2,3),
        MyList(4,5,6),
        MyList(7,8,9)
    )
    println(lsts)
    println(MyList.flatten(lsts))

    println(y.map((a:Int) => a * a))

    println(y.map(((a:Int) => MyList(a,a))))


}
