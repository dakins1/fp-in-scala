package parser

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {self =>

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def regex(r: Regex): Parser[String]
  val number = "[0-9]".r
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Primitives
  implicit def string(s: String): Parser[String]
  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  def product[A,B](p: Parser[A], p1: => Parser[B]): Parser[(A,B)] =
    p.flatMap(a1 => p1.map(a2 => (a1,a2)))

  // Primitive Combinators
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def many[A](a: Parser[A]): Parser[List[A]] =
    map2(a, many(a))(_ :: _) | succeed(Nil)

  def many1[A](a: Parser[A]): Parser[List[A]]  =
    map2(a, many(a))((a,as) => a :: as)

  // Combinators
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n == 0) succeed(Nil)
    else map2(p, listOfN(n-1,p))(_ :: _)
  }
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(a1 => succeed(f(a1)))

  val numA: Parser[Int] = char('a').many.slice.map(_.size)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String] // return portion of input examined if successful

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p ** p2 map f.tupled

  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a1 => p2.map(a2 => f(a1,a2)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def repeat[A](a: Parser[A]): Parser[List[A]] =
    number.flatMap(int => listOfN(int.toInt, a))


  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
  def map3[A,B,C,D](p: Parser[A], p2: => Parser[B], p3: => Parser[C])(f: (A,B,C) => D): Parser[D] =
    (p ** p2 ** p3).map(unbiasL).map(f.tupled)

  val whitespace = (char(' ') | char('\n')).many.slice

  def surround[A, B](o: Parser[A], c: Parser[A])(p: Parser[B]): Parser[(A, B, A)] =
    (o ** p ** c).map(unbiasL)

  def token[A](p: Parser[A]): Parser[A] =
    surround(whitespace, whitespace)(p).map(_._2)

  def separate[A,B](s: Parser[A])(p: Parser[B]): Parser[List[B]] = {
    val mult = (p ** s).many.map(_.map(_._1))
    val one = p.map(_ :: Nil)
    mult | one
  }

  val doubleString = token("[-+]?([0-9]*\\.)?[0-9]+".r)
  val double = doubleString.map(_.toDouble)

  /* Laws
  run(string(s))(s) == Right(s)
  map(p)(a => a) == p
  run(succeed(a))(s) == Right(a)

  Specifying behavior of product:
    run(succeed(a) ** succeed(a))(s) == Right(a,a)
    p1 ** p2 != p2 ** p1 if p1 != p2
    p1 ** (p2 ** p3) == (p1 ** p2) ** p3
    a.map(f) ** b.map(g) == (a ** b) map (case (a1,b1) => (f(a1),g(b1))
    a.flatMap(a1 => succeed(a1)) == a
   */

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A=>B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }

}

