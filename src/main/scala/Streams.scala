
package fpinscala.streams

sealed trait Stream[+A] {
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h,t) => h()::t().toList 
    }

    def take(n:Int):Stream[A] = this match {
        case Empty => Stream.empty
        case Cons(h, t) => 
            if (n == 0) Empty
            else Stream.cons(h(), t().take(n-1))
    }

    // Since all B arguments are =>, they are lazy and don't get evaluated
    // tail of this list does not get evaluated until later
    def foldRight[A1>:A,B](z: =>B)(f:(A, =>B) => B):B = this match {
        case Empty => z
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
    }

    def forAll(p:A => Boolean):Boolean = 
        this.foldRight(true)((a,b) => p(a) && b)

    // since || is non-strict, we never evaluate the tail of this function once f(a) is true, damn that's cool
    def exists(f:A => Boolean):Boolean = {
        this.foldRight(false)((a,b) => f(a) || b)
    }

    def takeWhile_fr(f: A => Boolean):Stream[A] = {
        this.foldRight(Stream.empty[A])((h,t) => {
            if (f(h)) Stream.cons(h,t)
            else Stream.empty
        })
    }

    def headOption:Option[A] = this match {
        case Empty => None
        case Cons(h,t) => Some(h())
    }

    def headOption_fr:Option[A] =
        this.foldRight(None.asInstanceOf[Option[A]])((h,opt) =>{
            //h is the head
            //t is this recursive result, will traverse the entire list
            //we really don't want to recurse at all
            Some(h)
        })

    def map[B](f:A => B):Stream[B] = {
        this.foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))
    }
    
    def filter(f:A => Boolean) = 
        this.foldRight(Stream.empty[A])((h,t) => {
            if (f(h)) Stream.cons(h, t)
            else t
        })

    def append[A1 >: A](x: => Stream[A1]):Stream[A1] = {
        this.foldRight(x)((h,t) => Stream.cons(h,t))
    }

    def flatMap[B](f:A => Stream[B]):Stream[B] = 
        this.foldRight(Stream.empty[B])((h,t) => {
            f(h).append(t)
        })


    // somehow this does not evaluate the list??!?!?! makes no sense
    // you literally have to force the function...
    // OHH, this doesn't actually run until the takeWhile result is referenced, now it makes sense
    def takeWhile(f:A => Boolean):Stream[A] = this match {
        case Empty => Empty
        case Cons(h,t) => { 
            if (f(h())) Stream.cons(h(), t().takeWhile(f))
            else Empty
        }
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:() => A, t:() => Stream[A]) extends Stream[A]
// t:() => Stream[A] kind of weird, but makes sense; a thunk to return the tail of the stream

object Stream {
    
    def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(()=> head, () => tail)
    }
    
    def empty[A]: Stream[A] = Empty

    def tabulate[A](n:Int)(f:Int => A):Stream[A] = {
        def _tab(n1:Int)(f:Int => A):Stream[A] = {
            if (n1 == n) Empty
            else {
                cons(f(n1), _tab(n1+1)(f))
            }
        }
        _tab(0)(f)
    }

    
    // I don't understand why this evaluates on construction... seems like it defeats the purpose of all this
    // Welp scala library does it the same, so obviously this is correct syntax. But wow, I reeeeaaalllly wish the author
        // had explained how #:: doesn't evalute on construction...
    // https://stackoverflow.com/questions/67550532/why-are-scalas-lazylists-elements-displayed-as-unevaluated-after-being-compute
    // Thank you stack overflow.
    // I can't believe the author didn't mention this! So confusing 
    // So we want to avoid apply method; instead use
        // tabluate
        // range
        // or #::
    def apply[A](as: A*):Stream[A] = 
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}