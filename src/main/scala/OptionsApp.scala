import scala.collection.immutable


object Options extends App {

    // Lift function f to work with options
    def map2[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
        case (Some(x), Some(y)) => Some(f(x,y))
        case _ => None
    }

    //Shame on me for using pattern matching
    def map22[A,B,C](a:Option[A], b:Option[B])(f: (A,B) => C): Option[C] = 
        a.flatMap(aa => b.map(bb => f(aa,bb)))

    def sequence[A](as:List[Option[A]]): Option[List[A]] =
        as.foldRight[Option[List[A]]](Some(Nil))((op, lst) => lst.flatMap(as => op.map(aa => aa::as)))

    def sequence2[A](as:List[Option[A]]): Option[List[A]] =
        as.foldRight[Option[List[A]]](Some(Nil))((op, lst) => map2(op, lst)(_ :: _))
    
    def sequence3[A](as:List[Option[A]]): Option[List[A]] = as match {
        case Nil => Some(Nil)
        case x::xs => x.flatMap(xx => sequence(xs).map(xx:: _))
    }

    val seq = (for (i <- 1 to 10) yield Some(i)).toList
    println(sequence(seq))
    println(sequence(seq:+None))
    println(sequence(None::seq))

    def Try[A](a: => A): Option[A] = 
        try Some(a)
        catch { case e: Exception => None }

    def traverse[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] = {
        a.foldRight[Option[List[B]]](Some(Nil))((a,acc) => acc.flatMap(lst => f(a).map(b => b::lst)))
    }

    def traverse2[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] = {
        a.foldRight[Option[List[B]]](Some(Nil))((a,acc) => map2(f(a),acc)(_ :: _))
    }

    def traverse3[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] = {
        a match {
            case head :: tl => f(head).flatMap(hh => traverse3(tl)(f).map(hh::_) )
            case Nil => Some(Nil)
        }
    }

    def traverse4[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
            case Nil => Some(Nil)
            case h::t => map2(f(h), traverse(t)(f))(_ :: _) // now this is some witchcraft
        }

    val intSeq = (for (i <- 10 to 20) yield i.toString()).toList
    val badSeq = intSeq:+"huh"
    val badSeq2 = None::intSeq
    println(traverse(intSeq)(a => Try{ a.toInt }))
    println(traverse2(intSeq)(a => Try{ a.toInt }))
    println(traverse3(badSeq)(a => Try{ a.toInt }))

    def sequenceViaTraverse[A](as:List[Option[A]]): Option[List[A]] = 
        traverse(as)(a => a)

    println(sequenceViaTraverse(seq))
    println(sequenceViaTraverse(seq:+None))
    println(sequenceViaTraverse(None::seq))

}
