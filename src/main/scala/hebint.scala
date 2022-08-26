import annotation.tailrec
object HEBInterview extends App {
    val s = "Hi, I would like to work at HEB!"
    val s2 = "Hello, team, nice to meet you. I like Scala."
    val s3 = "www.github.com/dakins1"
    val s4 = "And then he said, \"Oh no!!! What have I done?\"" //edge case of adjacent punctuation

    // Turns out the puncation ascii is more spread out than I thought. Could have also implemented this with a set.
    def isPunc(c:Char):Boolean = {
        ((32 <= c) && (c <= 47)) || 
        ((58 <= c) && (c <= 64)) ||
        ((91 <= c) && (c <= 96)) ||
        ((123 <= c) && (c <= 126)) &&
        c != 45 //skips '-' character for H-E-B
    }

    /*
    My new idea is to traverse the list until we hit a punctuation mark. Once we do, we then convert everything before 
    the punctuation to our expected output. Then, prepend this conversion to our current punctuation, and repeat
    for the rest of the list. By 'convert', I mean reverse every word while keeping them in place. E.g.
       Hi, I want to work at HEB!
         ^ stop here, convert "Hi", and prepend to ','
       iH, I want to work at HEB!  
                                ^ stop here, convert "I want to work at HEB", and prepend to '!'
       iH, I tnaw ot krow ta BEH!
                
    To code this, we have an accumulator that collects every element we've traversed thus far.
    Once we hit a character that is a punctation, we convert the accumulator and put it behind us. We then insert the 
    current puncutation mark and recurse down the list. We also reset our accumulator. That way, our accumulator doesn't
    contain the previous parts of the string we've already converted.

    We can streamline this a little bit, though. If you know your ASCII, you might have noticed the isPunc() 
    function considers a space character as punctuation. This will let us keep each word in its place since 
    we stop at spaces. Therefore, instead of writing a separate function to convert the accumulator, 
    we simply reverse whatever our accumulator is. Very nifty!
    
    And we can further streamline it with how we construct our accumulator. By building our accumulator with
    prepends, we reverse the accumulator as we traverse the list.
    We end up with what I consider a rather elegant solution, and in near linear time as well.  
    */
    def reverseWithPunctation(acc:List[Char], str:List[Char]):List[Char] = str match {
        case (c::cs) => {
            if (isPunc(c)) acc ++ (c::reverseWithPunctation(Nil, cs))
            else reverseWithPunctation(c::acc, cs)
        }
        case Nil => acc
    }

    // Here is the tail recursive solution. It is the same logic, except append our reversed accumulator as we
    // recurse. This increases the complexity of the function since we traverse the accumulator and builder for each punctuation mark.
    // In the first implementation, we would only re-traverse the accumulator for each punctuation. 
    // But for most input cases this would not create a noticeable difference. 
    @tailrec
    def reverseWithPunctation_TR(acc:List[Char], str:List[Char], builder:List[Char]):List[Char] = str match {
        case (c::cs) => {
            if (isPunc(c)) reverseWithPunctation_TR(Nil, cs, builder ++ acc:+ c)
            else reverseWithPunctation_TR(c::acc, cs, builder)
        }
        case Nil => builder ++ acc
    }

    println("Regular recursion: ")
    println(reverseWithPunctation(Nil, s.toList).mkString)
    println(reverseWithPunctation(Nil, s2.toList).mkString)
    println(reverseWithPunctation(Nil, s3.toList).mkString)
    println(reverseWithPunctation(Nil, s4.toList).mkString)
    println("tailrec: ")
    println(reverseWithPunctation_TR(Nil, s.toList, Nil).mkString)
    println(reverseWithPunctation_TR(Nil, s2.toList, Nil).mkString)
    println(reverseWithPunctation_TR(Nil, s3.toList, Nil).mkString)
    println(reverseWithPunctation_TR(Nil, s4.toList, Nil).mkString)


    // Here is the implementation of the approach we attempted on the call. This was not nearly as simple as I had hoped.
    
    // Don't include space characters this time
    def isPunc2(c:Char):Boolean = {
        ((33 <= c) && (c <= 47)) || 
        ((58 <= c) && (c <= 64)) ||
        ((91 <= c) && (c <= 96)) ||
        ((123 <= c) && (c <= 126)) &&
        c != 45 //skips '-' character for H-E-B
    }

    // Strip all punctuation while marking down the index of that punctuation
    // Realistically it would be much more readable to implement the stripping and map building into separate functions, and just
    // traverse the list two times.
    def stripPunc (ls:List[Char]):(Map[Int,Char], List[Char]) = {
        val (map, _, noPunc) =  ls.foldLeft((Map.empty[Int,Char], 0, Nil.asInstanceOf[List[Char]])){
            case ((map, ind, builder), c) => {
                if (isPunc2(c)) { 
                    ((map+(ind -> c)), (ind+1), builder) 
                }
                else (map, ind+1, c::builder)
            }}
        map -> noPunc.reverse
    }
    
    // Here we re-insert the punctuation into the string that has been appropriately reversed
    def rebuild(revvedNoPunc:List[Char], ind:Int, mapping:Map[Int,Char]):List[Char] = revvedNoPunc match {
        case c::cs => mapping
            .get(ind)
            .map(punc => punc::rebuild(c::cs, ind+1, mapping))
            .getOrElse(c::rebuild(cs, ind+1, mapping))
        case Nil => mapping.get(ind).map(punc => List(punc)).getOrElse(Nil)
    }

    // put it all together
    def flip(s:String) = {
        val (mapping, noPunc) = stripPunc(s.toList)
        // very painful necessity of converting the string into a list, mapping reverse to each word, then re-inserting 
        // a space between each word. Scala's split() functions weren't as useful as I had hoped -- I forgot it removes
        // the character you've split at
        val revWords = noPunc.mkString.split(" ").toList.map(_.reverse).mkString(" ").toList
        rebuild(revWords, 0, mapping)
    }

    println("Rebuilder method: ")
    println(flip(s).mkString)
    println(flip(s2).mkString)
    // And after all that, this method breaks for the github input since there aren't any spaces in the string. I did not try to fix
    // the solution to handle this edge case. You would have to write your own split at function that splits at any instance of a list of characters,
    // and pass all punctuation in that list...which would get us very close to the first solution.
    println(flip(s3).mkString)
    println(flip(s4).mkString)
}
