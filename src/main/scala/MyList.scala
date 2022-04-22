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
     * I expected to only need [A,B], but contravariance comes into play here (I believe)
     * definition: 
         if A is a subtype of B, then GenClass[B] is a subtype of GenClass[A]
     * If we define this func as accepting a supertype of A, then
         A is a subtype of A1
         func[A1] is a subtype of func[A]
         and this resolves the types needed to compile successfully
     */
    @tailrec
    final def foldLeft[A1 >: A,B](z:B)(f: (A1,B) => B):B = this match {
            case Nil => z
            case Cons(x,xs) => xs.foldLeft(f(x,z))(f)
        }

    final def reverse[A1>:A]():MyList[A1] = this.foldLeft(Nil:MyList[A1])(Cons(_,_))

    final def foldRight[A1>:A,B](z:B)(f:(A1,B) => B):B = this.reverse.foldLeft(z)(f)

    final def foldLeftViaFoldRight[A1>:A,B](z:B)(f:(A,B) => B):B = this.reverse.foldRight(f(this.head,z))(f)

    def foldRight_nonTailRec[A1 >: A,B](z:B)(f: (A1,B) => B):B = this match {
            case Nil => z
            case Cons(x,xs) => f(x, xs.foldRight(z)(f))
        }

    def append[A1 >: A](as:MyList[A1]):MyList[A1] = this.foldRight(as)(Cons(_,_))

    def map[A1 >: A,B](f: (A1)=>B):MyList[B] = this.foldRight(Nil:MyList[B])((x,xs) => Cons(f(x),xs))

    def headOption_fr:Option[A] =
        this.foldRight(None.asInstanceOf[Option[A]])((h,opt) => Some(h))


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

    def flatten[A](lists:MyList[MyList[A]]):MyList[A] = {
        lists.foldRight(Nil:MyList[A])((l1,l2) => l1.append(l2))
    }
    
}
