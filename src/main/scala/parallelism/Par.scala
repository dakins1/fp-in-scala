package fpinscala.mystuff.parallelism

import java.util.concurrent._


object Par {
    
    type Par[A] = ExecutorService => Future[A]

    private case class UnitFuture[A](get:A) extends Future[A] {
        def isDone = true
        def get(timeout:Long, units:TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning:Boolean):Boolean = false
    }

    def map2[A,B,C](a:Par[A], b:Par[B])(f:(A,B) => C):Par[C] = // combine result of two parallel computations
        (es: ExecutorService) => {
            val af = a(es)
            val bf = b(es)
            UnitFuture(f(af.get, bf.get))
        }
    def unit[A](a:A):Par[A] = ??? //take unevaluated A and return a computer that that migh evaluate in a separate thread
                                        // a unit of parallelism that wraps a single value

    def run[A](s:ExecutorService)(a:Par[A]):Future[A] = a(s) // for extracting the resuling value from a Par by actually performing the computation

    def fork[A](a: => Par[A]):Par[A] = // marks the given computation for concurrent evaluation. Eval won't occur until forced by run
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })

    def lazyUnit[A](a: => A):Par[A] = fork(unit(a)) // derived combinator - defined in terms of other operations

    // convert a function to evaluate asynchronously 
    def asyncF[A,B](f:A=>B): A => Par[B] =
        a => lazyUnit(f(a))
    
    def map[A,B](a:Par[A])(f:A=>B):Par[B] = 
        map2(a, unit(()))((a,_) => f(a))

    def sortPar(parList:Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    def sequence[A](ps:List[Par[A]]): Par[List[A]] = // do not call run
        ps.foldRight[Par[List[A]]](unit(Nil))((p, acc) => map2(p, acc)((p1,ls) => p1::ls))


    def parMap[A,B](ps:List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
    }

    // filters a list of elements in parallel .... doesn't necessarily evaluate the boolean in parallel
    // but of course, we should evalute each filter in parallel
    def parFilter[A](l:List[A])(f:A => Boolean): Par[List[A]] =     {
        val pars: List[Par[List[A]]] =
            l.map(asyncF(a => if (f(a)) List(a) else List())) // lifting to individual lists helps with filtering out false values
        map(sequence(pars))(_.flatten)
    }   

    def sum(ints: IndexedSeq[Int]): Par[Int] = {
        if (ints.size <= 1)
            Par.unit(ints.headOption getOrElse 0) // wrap in unit to make types happy
        else {
            val (l,r) = ints.splitAt(ints.length/2)
            Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)
        }
    }

    def delay[A](fa: => Par[A]):Par[A] = //avoids deadlocks, but computation still gets executed in the main thread
        es => fa(es)


}
