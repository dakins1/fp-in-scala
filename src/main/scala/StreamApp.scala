import fpinscala.streams._

object StreamApp extends App {
  // val x = Stream({println("bruh"); 1+1}, {println("bruhhh"); 2+2})
  // val y = x.toList
  // Dummy explicity, won't evaluate any of these
  println()
  println("explicit constructor, nothing should print...")
  val x1 = Cons(() => {println("bruh");  () => 1+1}, () => Cons({println("bruhhh"); () => 2+2}, () => Empty))
  println("...done")
  println()
  println("toList forces, will print bruh as expected")
  val x2 = x1.toList
  println()
  // If we use cons smart constructor, we'll evaluate on construction, but cache values for l8r
  println("smart cons forces")
  val x3 = Stream({println("bruh"); 1+1}, {println("bruhhh"); 2+2}, {println("bruhhhhh"); 4+4})
  // But if we call toList, it won't get reevaluated again
  println("Nothing should print...")
  val x4 = x3.toList
  println("...toList finished")
  println()
  
  println("Scala library stream")
  val y = scala.collection.immutable.Stream({println("boi"); 12}, {println("grill"); 42})
  println("...done")
  println()

  def expensive():Int = {
    println("oof")
    7
  }

  def expensive(n:Int):Int = {
    println("exp: " + n)
    n
  }
  println("Scala LazyList library")
  val z = scala.collection.immutable.LazyList({println("boi"); 12}, {println("grill"); 42})
  val z1 = scala.collection.immutable.LazyList(expensive(), expensive())
  val z2 = scala.collection.immutable.LazyList.from(0)
  println()

  println("constructing...")
  val fibs: LazyList[BigInt] =
    {println("boi");BigInt(0)} #:: {println("grill"); BigInt(1) }#::
      fibs.zip(fibs.tail).map{ n =>
        println(s"Adding ${n._1} and ${n._2}")
        n._1 + n._2
      }
  println("...done")
  println()

  println("takes")
  fibs.take(5).foreach(println)
  fibs.take(6).foreach(println)
  println()

  println("my take")
  val q = Stream(expensive(), expensive(), expensive())
  println(q.take(2))
  println()

  println("q2 apply")
  val q2 = Stream(expensive(2), expensive(4), expensive(6), expensive(7), expensive(8))
  // println(q2.takeWhile(x => {println(x); x % 2 == 0}))
  println("q2 take while")
  val q2_5 = q2.takeWhile(x =>  x % 2 == 0)

  println("lazylist tabulate")
  val q3 = LazyList.tabulate(10)(e => expensive(e))
  println(q3)
  println("takeWhile")
  val q4 = q3.takeWhile(_ < 5)
  println(q4)
  println("tolist")
  q4.toList
  println(q4.toList)

  println("stream tab")
  val p = Stream.tabulate(10)(s => expensive(s))
  println("take while ...")
  val p1 = p.takeWhile(_ < 5)
  println("...tooken")
  val p2 = p1.toList
  println(p2)

  println(p.forAll(x => x < 9))

  println("p3")
  val p3 = Stream.tabulate(10)(s => expensive(s))
  println("done tabulating")
  val p4 = p3.takeWhile_fr(_ < 7)
  println("done take while")
  println(p4.toList)

  val p5 = Stream.tabulate(10)(s => expensive(s))
  println(p5.headOption_fr)
  val p6 = Stream()
  println(p6.headOption_fr)
  val p7 = p5.map(_ * 2)
  println(p7.toList)

  val p8 = Stream.tabulate(10)(s => expensive(s))
  println(p8.filter(_ %2 ==0).toList)

  val p9 = Stream.tabulate(10)(s => expensive(s))
  println(p9.flatMap(x => Stream.tabulate(2)(x1 => x+x1)).toList)
}
