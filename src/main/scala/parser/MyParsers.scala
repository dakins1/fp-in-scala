package parser

trait MyParsers[ParseError, Parser[+_]] { self =>
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]


  def map[A,B,C](s1: Parser[A], s2: Parser[B])(f: (A,B) => C): Parser[C]
  // Define what it means to map: combine two recognizers into once recognizer
  // might have to define different maps for different types of parsers...
  // i think i'm actually realizing this is an misnomer for concat...

  def mapCounter[Int](s1: Parser[Int], s2: Parser[Int]): Parser[(Int,Int)] =
    map(s1, s2)(Tuple2.apply)


  // Primitives
  def char(c: Char): Parser[Char] // recognizes a single char
  def emptyString: Parser[String] // recognizes the empty string
  def chars(cs: List[Char]): Parser[List[Char]] // recognizes a list of chars and returns the list of chars found
  def atLeastOnce[A](p: Parser[A]): Parser[Int] // recognizes a pattern and returns a count, returns error if none found
  def zeroOrMore[A](p: Parser[A]): Parser[Int] // recognizes zero or more patterns and returns count
  def atLeast[A](n: Int, p: Parser[A]): Parser[Int]
  // Because failing or not on a zero count is determined by the Parser, the distinction has to come from primitives. We can't inspect
    // the internals of Parser, i.e. see if it threw a ParserError, without running it. Since it's pushed down, there's no way for us to build one up
    // with combinators. Thus it has to be primitive
  // Also, since Parser[Int] will behave differently from a Parser[List[Char]], it has to be primitively different
  // To tie in the Good Math, here we are first defining what laws this algebra has to obey. Then we will come up with
    // and implementation. But this parser will exist only because it obeys the laws we have defined.
  // And although we are making a primitive from another primitive, it still feels primitive all around. Maybe a subtype
    // would work here

  // Strictly combinators
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def succeed[A](a: A): Parser[A] =
    map(string(""))(_ => a)
  def ors[A](cs: List[A])(implicit f: A => Parser[A]): Parser[A] = cs.tail.foldLeft(f(cs.head))((acc,c) => or(acc,f(c)))
  def concat[A,B](p1: Parser[A], p2: Parser[A])(f: (A,A) => B): Parser[B]
    // In general, we sequentially identify these patterns, then provide a combinator for how to assemble the output from both parsers should they succeed
  def concatString(s1: Parser[String], s2: Parser[String]): Parser[String] =
    concat(s1,s2)(_+_)
  def concatCounter(c1: Parser[Int], c2: Parser[Int]): Parser[(Int,Int)] =
    concat(c1,c2)(Tuple2.apply)

  /* Laws
  map(p)(a => a) == p
  run(succeed(a))(s) == Right9)

  run(concatString(string(s1),string(s2)))(s1 + s2) == Right(s1 + s2)
  run(concatString(string(s1),string(s2)))(s3 + s1 + s2) == Right(s1 + s2)
  run(concatCount(atLeastOnce(s1),zeroOrMore(s2)))(s1 + s2) == Right(1,1)

  a | b == b | a <- which implies a | (b | c) == (a | b) | c

   */

  // Primitive parse errors
  def parseError(msg: String): ParseError // Return a ParseError containing the message provided

  // ParseError combinators
  // either go dummy simple and the programmer passes in a string of the error message
      // or you go balls to the walls and define a bunch of data members of ParseError, and write combinators
      // that combine error messages, e.g. (expected one or more 'aa')


  def many[A](p: Parser[A]): Parser[List[A]]
  // map from above

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))
  implicit def charParserOps[A](c: Parser[Char]): ParserOps[Char] = ParserOps[Char]
  implicit def charParserOps[A](c: Parser[Int]): ParserOps[Int] = ParserOps[Int]
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
  }

}

object Parsers {
  /* Laws

  Try to come up with a minimal set of primitives, and come up with general laws
  add'tl parsing tasks:
    Parser[Int] that recognizes zero or more 'a' characters
    Parser[Int] that recognizes one or more 'a' characters; fails on zero

    I think we want to make counting a primitive type
    The other primitive type...a tracker? Recognizer? Return-er
    uhh.. Are the various forms of repetition primitive in our algebra, or could they be defined in terms of something simpler?


    Do something about the ParseError too...


    doing something about building up a list of char just to throw away the list/length

    Parser is just the thing that stores the logic of what to look for, and spits out what it was looking for

    also, so far we've only discussed combinators

    Law: - think of laws in terms of variables - need to think about what a primitive in an algebra is
    e.g. think of how a map can take a primitive and apply it to a list of things, vs. making a List primitive
    we don't yet care how Parser works under the hood, we just want to think about what functionality, specifically primitive functionality, we want
    Then combinators are made by using some combination of primitives
    run(string(s))(s) == Right(s)

    counter: could just be a map/flatmap/fold of a single Parser[Char]. And it tickers away the input string
    oh bruh, a primitive is an actual type. Like Parser[Int]. a combinator is a function that produces a primitive, or something
   */
  /*
  The Parser class is just an object for storing the data of what pattern you want to recognize
   */


  val x = List(1)
  val y = 4 :: x
}
