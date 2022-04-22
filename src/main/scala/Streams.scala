
package fpinscala.streams

sealed trait Stream[+A] {
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h,t) => h()::t().toList 
    }

    def tail:Stream[A] = this match {
        case Empty => throw new scala.Exception("empty list")
        case Cons(h,t) => t()
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
        this.foldRight(None.asInstanceOf[Option[A]])((h,opt) => Some(h))

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

    def find(f:A => Boolean):Option[A] =
        this.filter(f).headOption
        // this will break out as soon as something is found

    def mapViaUnfold[S](f: A => S):Stream[S] = 
        Stream.unfold(this)(s => s match {
            case Empty => None
            case Cons(h,t) => Some(f(h()) -> t() )
        })
    
    def takeViaUnfold(n:Int):Stream[A] = 
        Stream.unfold((this,0)){case (str, cnt) => {
            if (cnt >= n) None
            else { 
                str match {
                    case Empty => None
                    case Cons(h, t) => {
                        val state = t() -> (cnt+1)
                        Some(h() -> state)
                    }
                }
            } 
        }}

    def takeWhileViaUnfold(f:A => Boolean):Stream[A] =
        Stream.unfold(this)(s => s match {
            case Cons(h, t) if (f(h())) => Some(h(), t())
            case _ => None
        })
    
    def zipWithViaUnfold[B,C](s:Stream[B])(f:(A,B) => C):Stream[C] = 
        Stream.unfold((this,s)){case (s1,s2) => (s1,s2) match {
            case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()) -> (t1() -> t2()))
            case _ => None
        }}

    def zipAll[B](s2:Stream[B]):Stream[(Option[A],Option[B])] =
        Stream.unfold((this, s2)){ case (s1, s2) => (s1,s2) match {
            case (Cons(h1,t1),Cons(h2,t2)) => {
                Some( (Some(h1()) -> Some(h2())) -> (t1() -> t2()) )
            }
            case (Cons(h1,t1),Empty) => {
                Some( (Some(h1()) -> None) -> (t1() -> Empty) )
            }
            case (Empty, Cons(h2,t2)) => {
                Some( (None -> Some(h2())) -> (Empty -> t2()) )
            }
            case _ => None
        }}

    def hasSubsequence[A1>:A](s2:Stream[A1]):Boolean = {
        Stream.unfold(this)(s => s match {
            case Empty => None
            case Cons(h,t) => {
                lazy val eval = Cons(h,t).zipAll(s2)//((h1,h2) => (h1->h2))
                Some(eval -> t())
            }
        })
        .map(_
            .map(pair => (for {
                h1 <- pair._1
                h2 <- pair._2
            } yield h1 == h2))
            .takeWhile(opt => opt match {
                case Some(x) => true
                case None => false
            }).forAll(_.getOrElse(false))
        ).exists(_ == true)
            // pair._1 == pair._2))
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

    def constant[A](a:A):Stream[A] = 
        Stream.cons(a, constant(a))

    def betterConstant[A](a:A):Stream[A] = {
        lazy val tail:Stream[A] = Cons(() => a, () => tail)
        tail
    }

    def from(n:Int):Stream[Int] = 
        cons(n, from(n+1))

    // there should never be an empty - that's a base case we'll never reach
    def fibs:Stream[Int] = {
        def _fibs(prev1:Int, prev2:Int):Stream[Int] = {
            cons(prev1, _fibs(prev2, prev1+prev2))
        }
        _fibs(0,1)
    }

    // stream builder
    // takes initial state, and a function for producing both the next state and 
        // the next value in the generate stream
    def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = {
        f(z).map{case (a,s) => 
            cons(a, unfold(s)(f))
        }.getOrElse(empty)
    }

    def onesViaUnfold:Stream[Int] = 
        unfold(1)(_ => Some(1, 1))
    
    def constantViaUnfold[A](a:A) = 
        unfold(a)(_ => Some(a, a))
    
    def fromViaUnfold(n:Int):Stream[Int] = 
        unfold(n)(i => Some(i, i+1))

    def fibsViaUnfold:Stream[Int] =
        unfold((0,1)){case (x,y) => Some(x -> (y, x+y))}



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