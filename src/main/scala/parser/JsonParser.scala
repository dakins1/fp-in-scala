package parser
import parser.Parsers
import parser.JSON._

import scala.util.matching.Regex

object JsonParser {
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P.{string => _, regex => _, _}
    implicit def tok(s: String) = token(P.string(s))
    implicit def reg(r: Regex) = token(P.regex(r))
    // Structural

    val sstring = "\".*\"".r.map(_.drop(1).dropRight(1))

    def repeated[A](p: Parser[A], delimChar: Char): Parser[List[A]] =
      (p ** token(delimChar)).map(_._1).many1
    def repeatedOrOnce[A](p: Parser[A], delimChar: Char): Parser[List[A]] =
      repeated(p, delimChar) | p.map(_ :: Nil)
    def repeatedOrOnceOrNone[A](p: Parser[A], delimChar: Char): Parser[List[A]] =
      (p ** token(delimChar)).map(_._1).many | p.map(_ :: Nil) | whitespace.map(_ => Nil)


    // Values
    val jString = sstring.map(JString)
    val jNumber = double.map(JNumber)
    val jBool = ("true" | "false").map(_.toBoolean).map(JBool)
    val jNull = token("null").map(_ => JNull)
    val literal: Parser[JSON] = jString | jNumber | jBool | jNull
    val keyVal = (sstring ** value).map{ case (str, json) => Map(str -> json)}
    def jArray = surround("[","]")(separate(",")(value)).map(vs => JArray(vs._2.toIndexedSeq))
    def value: Parser[JSON] = literal | jArray | jsonObject
    def jsonObject = surround[String, Map[String, JSON]]("{","}")(keyVal).map(t => JObject(t._2))

    jsonObject
  }
}
