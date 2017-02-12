import fpinscala.errorhandling._

val x: Option[Int] = Some(7)
val y: Option[Int] = None

val mx = x.map(_ + 1)
val my = y.map(_ + 1)
val goex = x.getOrElse(0)
val goey = y.getOrElse(0)

val test: Int => Option[String] = { i =>
  if (i > 5) Some(i.toString)
  else None
}

val fmx = x.flatMap(test)
val fmy = y.flatMap(test)

val oex = x.orElse(Some(3))
val oey = y.orElse(Some(3))

val fltrx1 = x.filter(_ > 4)
val fltrx2 = x.filter(_ > 9)
val fltry1 = y.filter(_ > 4)
val fltry2 = y.filter(_ > 9)

val xs = Seq(1.0, 2.0, 3.0, 4.0, 5.0)

Option.variance(xs)

val zs1 = List(10, 20, 30, 40)
val zs2 = List(10, 8, 6, 4, 2)

val mzs1 = zs1.map(test)
val mzs2 = zs2.map(test)

Option.sequence(mzs1) == Option.traverse(zs1)(test)
Option.sequence(mzs2) == Option.traverse(zs2)(test)

Option.sequenceViaTraverse(mzs1) == Option.sequence(mzs1)
Option.sequenceViaTraverse(mzs2) == Option.sequence(mzs2)
