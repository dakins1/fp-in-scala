import fpinscala.streams._
import fpinscala.mystuff.MyList

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

  val p10 = MyList(expensive(0),expensive(1),expensive(2),expensive(3),expensive(4),expensive(5))
  println(p10.headOption_fr)

  val p11 = Stream.tabulate(10)(s => expensive(s))
  println(p11.find(_ > 5))

  // infinite streams
  val ones: Stream[Int] = Stream.cons(expensive(1), ones.map(x => {println("map: " + x); x+1})) //spooky
  println(ones.take(5).toList)
  val bs = Stream.constant('b')
  println(bs.take(5).toList)
  
  val f = Stream.fibs.take(10)
  println(f.toList)

  println(Stream.fromViaUnfold(5).take(7).toList)

  println(Stream.fibsViaUnfold.take(10).toList)

  // more corecursion 
  val t1 = Stream.tabulate(10)(s => expensive(s))
  println(t1.mapViaUnfold(_ * 2).take(5).toList)
  println("take via unfold")
  println(Stream.tabulate(10)(s => expensive(s)).takeViaUnfold(5).toList)
  println("take while via unfold")
  println(Stream.tabulate(10)(s => expensive(s)).takeWhileViaUnfold(_ < 5).toList)
  val t2 = Stream.tabulate(10)(s => expensive(s))
  val t3 = Stream.tabulate(10)(s => expensive(s))
  println(t2.zipWithViaUnfold(t3)(_+_).toList)
  val t4 = Stream.tabulate(10)(s => expensive(s))
  val t5 = Stream.tabulate(5)(s => expensive(s))
  println(t5.zipAll(t4).toList)
  val t6 = Stream.tabulate(5)(s => expensive(s))
  println(t6.tail.toList)
  def expensive2(n:Int) = {
    println("exp2: " + (n+1))
    n+1
  }
  val t7 = Stream.tabulate(10)(s => expensive(s))
  val t8 = Stream.tabulate(7)(s => expensive2(s))
  println(t7.hasSubsequence(t8))
  println(t8.hasSubsequence(t7))
  val t9 = Stream.tabulate(10)(s => expensive(s))
  val t10 = Stream.tabulate(6)(s => expensive(s))
  println(t10.hasSubsequence(t9))

  
}
