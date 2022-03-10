package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait MyList[+A] {
    def name():Unit = println("bruh")
    def length[A]():Int = this match {
        case Nil => 0
        case Cons(x,xs) => 1 + xs.length()
    }
    def sum[B >: A](implicit num: math.Numeric[B]): B = this match {
        case Nil => 0
        case Cons(x:B, xs:MyList[B]) => 1 + xs.sum
    }
}
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

    def sum(ints: MyList[Int]):Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }

    def tail[A](l: MyList[A]): MyList[A] =
        l match {
            case Nil => sys.error("tail of empty list")
            case Cons(_,t) => t
        }

    def head[A](as: MyList[A]):A = as match {
        case Nil => sys.error("head of empty list")
        case Cons(x,xs) => x
    }

    def foldRight[A,B](as: MyList[A], z:B)(f:(A,B) => B):B = 
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    
    @tailrec def foldLeft[A,B](as: MyList[A], z:B)(f:(A,B) => B):B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(x,z))(f)
        }

    def lengthFL[A](as: MyList[A]):Int = 
        foldLeft(as, 0)((_,acc) => acc+1)

    def reverse[A](as: MyList[A]):MyList[A] =
        foldLeft(as, Nil:MyList[A]) ( (a,b) => Cons(a, b) )
    

    def apply[A](as: A*): MyList[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    
}
