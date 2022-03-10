package fpinscala
package mystuff

import scala.annotation.tailrec

sealed trait MyList[+A] {
    def head:A
    def tail:MyList[A]

    def length():Int = {
        @tailrec
        def _length[A](as: MyList[A], count:Int = 0):Int = as match {
            case Nil => count
            case Cons(x,xs) => _length(xs, count+1)
        }
        _length(this)
    }

    /**
     * Expected to only need [A,B], but contravariance comes into play here I believe
     * definition: 
         if A is a subtype of B, then GenClass[B] is a subtype of GenClass[A]
     * If we define this func as accepting a supertype of A, then
         A is a subtype of A1
         func[A1] is a subtype of func[A]
         and this resolves the types needed to compile successfully
     */
    def foldRight[A1 >: A,B](z:B)(f: (A1,B) => B):B = {
        // @tailrec
        def _foldRight[A,B](as: MyList[A], z:B)(f:(A,B)=>B):B = as match {
            case Nil => z
            case Cons(x,xs) => f(x, _foldRight(xs, z)(f))
        }
        _foldRight(this, z)(f)
    }

    @tailrec
    final def foldLeft[A1 >: A,B](z:B)(f: (A1,B) => B):B = this match {
            case Nil => z
            case Cons(x,xs) => xs.foldLeft(f(x,z))(f)
        }

}

case class Cons[+A](head:A, tail:MyList[A]) extends MyList[A] {
    
}

case object Nil extends MyList[Nothing] {
    def head = throw new NoSuchElementException("head of empty list")
    def tail = throw new UnsupportedOperationException("tail of empty list")
}

object MyList {
    def apply[A](as: A*): MyList[A] = 
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}
