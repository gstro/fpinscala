package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Left => _, Right => _, Option => _, None => _, Some => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter


class ErrorHandlingTest extends FlatSpec with Matchers {

  "Map on Option" should "transform Some" in {
    Some(7).map(_ + 1) shouldEqual Some(8)
  }

  it should "not transform None" in {
    (None: Option[Int]).map(_ + 1) shouldEqual None
  }

  "Get or else on Option" should "provide inner value of Some" in {
    Some(7).getOrElse(0) shouldEqual 7
  }

  it should "provide default value on None" in {
    (None: Option[Int]).getOrElse(0) shouldEqual 0
  }

  val transformingFunc: Int => Option[String] = { i =>
    if (i > 5) Some(i.toString)
    else None
  }

  "FlatMap on Option" should "transform Some value" in {
    Some(7).flatMap(transformingFunc) shouldEqual Some("7")
  }

  it should "not transform None value" in {
    (None: Option[Int]).flatMap(transformingFunc) shouldEqual None
  }

  "Or Else on Option" should "return same option on Some" in {
    Some(7).orElse(Some(3)) shouldEqual Some(7)
  }

  it should "supply provided default values on None" in {
    (None: Option[Int]).orElse(Some(3)) shouldEqual Some(3)
  }

  "Filter on Option" should "keep value on matched Some" in {
    Some(7).filter(_ > 4) shouldEqual Some(7)
  }

  it should "provide None on unmatched Some" in {
    Some(7).filter(_ > 9) shouldEqual None
  }

  it should "provide None on any function on None" in {
    (None: Option[Int]).filter(_ > 4) shouldEqual None
  }

  "Variance on Option Sequence" should "provide correct calculation for Seq-Some" in {
    Option.variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) shouldEqual Some(2.0)
  }

  it should "give None value for Empty Seq" in {
    Option.variance(Seq[Double]()) shouldEqual None
  }

  "Sequence on mapped options" should "equal a traverse operation for increasing list" in {
    val seq = List(10, 20, 30, 40)
    Option.sequence(seq.map(transformingFunc)) shouldEqual Option.traverse(seq)(transformingFunc)
  }

  it should "equal a traverse operation on decreasing list" in {
    val seq = List(10, 8, 6, 4, 2)
    Option.sequence(seq.map(transformingFunc)) shouldEqual Option.traverse(seq)(transformingFunc)
  }
}
