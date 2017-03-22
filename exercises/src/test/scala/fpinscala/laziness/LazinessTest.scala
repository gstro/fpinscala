package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}


class LazinessTest extends FlatSpec with Matchers {

  "toList" should "give the corresponding list on non-empty stream" in {
    Stream(1, 2, 3, 4).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "give the empty list with the correct type on empty stream" in {
    Stream.empty[Int].toList shouldEqual List.empty[Int]
  }

  "take 4" should "give 4 elements when stream has more elements" in {
    Stream(1, 2, 3, 4, 5).take(4).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "give 3 elements when stream has fewer elements" in {
    Stream(1, 2, 3).take(4).toList shouldEqual List(1, 2, 3)
  }

  it should "give empty stream when stream is empty" in {
    Stream.empty[Int].take(4).toList shouldEqual List.empty[Int]
  }

  "drop 4" should "return final elements when stream has more elements" in {
    Stream(1, 2, 3, 4, 5, 6).drop(4).toList shouldEqual List(5, 6)
  }

  it should "return empty stream when stream has fewer elements" in {
    Stream(1, 2, 3).drop(4).toList shouldEqual List.empty[Int]
  }

  it should "return empty stream on empty stream" in {
    Stream.empty[Int].drop(4).toList shouldEqual List.empty[Int]
  }

  "takeWhile < 5" should "return the first 4 elements of Stream(1, 2, 3, 4, 5, 6)" in {
    Stream(1, 2, 3, 4, 5, 6).takeWhile(_ < 5).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "return the full stream when all elements match" in {
    Stream(1, 2, 3).takeWhile(_ < 5).toList shouldEqual List(1, 2, 3)
  }

  it should "return empty stream when no elements match" in {
    Stream(11, 12, 13).takeWhile(_ < 5).toList shouldEqual List.empty[Int]
  }

  it should "return empty stream on empty stream" in {
    Stream.empty[Int].takeWhile(_ < 5).toList shouldEqual List.empty[Int]
  }

  "forAll < 5" should "return true for elements that match" in {
    Stream(1, 2, 3).forAll(_ < 5) shouldBe true
  }

  it should "return false for a stream that contains elements don't match" in {
    Stream(1, 2, 3, 4, 5, 6).forAll(_ < 5) shouldBe false
  }

  it should "return true on an empty stream" in {
    Stream.empty[Int].forAll(_ < 5) shouldBe true
  }

  "takeWhile2 < 5" should "return the first 4 elements of Stream(1, 2, 3, 4, 5, 6)" in {
    Stream(1, 2, 3, 4, 5, 6).takeWhile2(_ < 5).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "return the full stream when all elements match" in {
    Stream(1, 2, 3).takeWhile2(_ < 5).toList shouldEqual List(1, 2, 3)
  }

  it should "return empty stream when no elements match" in {
    Stream(11, 12, 13).takeWhile2(_ < 5).toList shouldEqual List.empty[Int]
  }

  it should "return empty stream on empty stream" in {
    Stream.empty[Int].takeWhile2(_ < 5).toList shouldEqual List.empty[Int]
  }

  "headOption" should "return Some for non-empty stream" in {
    Stream(1, 2, 3).headOption shouldEqual Some(1)
  }

  "headOption" should "return Some for stream of one element" in {
    Stream(1).headOption shouldEqual Some(1)
  }

  it should "return None for empty stream" in {
    Stream.empty[Int].headOption shouldEqual None
  }

  "map length" should "transform a stream of Strings" in {
    Stream("one", "two", "three", "four").map(_.length).toList shouldEqual List(3, 3, 5, 4)
  }

  it should "return empty stream of type Int on empty stream" in {
    Stream.empty[String].map(_.length).toList shouldEqual List.empty[Int]
  }

  "filter < 5" should "return the correct stream on non-empty stream" in {
    Stream(1, 2, 3, 4, 5, 6).filter(_ < 5).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "return empty stream when applied to empty stream" in {
    Stream.empty[Int].filter(_ < 5).toList shouldEqual List.empty[Int]
  }

  it should "return empty stream when applied to non-empty stream without matches" in {
    Stream(11, 12, 13, 14, 15, 16).filter(_ < 5).toList shouldEqual List.empty[Int]
  }

  "append Stream(4)" should "return extended stream with value at the end" in {
    Stream(1, 2, 3).append(Stream(4)).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "return single element stream on empty stream" in {
    Stream.empty[Int].append(Stream(4)).toList shouldEqual List(4)
  }

  "flatMap Stream(_.length)" should "transfor a stream of strings" in {
    Stream("one", "two", "three", "four").flatMap(w => Stream(w.length)).toList shouldEqual List(3, 3, 5, 4)
  }

  it should "return empty stream of type Int on empty stream" in {
    Stream.empty[String].flatMap(w => Stream(w.length)).toList shouldEqual List.empty[Int]
  }

  "constant 4" should "generate 7 4s after take(7)" in {
    Stream.constant(4).take(7).toList shouldEqual List(4, 4, 4, 4, 4, 4, 4)
  }

  it should "generate an empty list after take(0)" in {
    Stream.constant(4).take(0).toList shouldEqual List.empty[Int]
  }

  "constant 'hi'" should "generate 7 'hi's after take(7) (type test)" in {
    Stream.constant("hi").take(7).toList shouldEqual List("hi", "hi", "hi", "hi", "hi", "hi", "hi")
  }

  "from" should "generate 4 5 6 7 after take(4) when supplied initial value 4" in {
    Stream.from(4).take(4).toList shouldEqual List(4, 5, 6, 7)
  }

  it should "generate -1 0 1 2 after take(4) when supplied initial value -1" in {
    Stream.from(-1).take(4).toList shouldEqual List(-1, 0, 1, 2)
  }

  "fibs take 7" should "generate the first 7 elements of the Fibonacci sequence" in {
    Stream.fibs.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  "ones2 take 4" should "generate 4 1s values" in {
    Stream.ones2.take(4).toList shouldEqual List(1, 1, 1, 1)
  }

  "constant2 4" should "generate 7 4s after take(7)" in {
    Stream.constant2(4).take(7).toList shouldEqual List(4, 4, 4, 4, 4, 4, 4)
  }

  it should "generate an empty list after take(0)" in {
    Stream.constant2(4).take(0).toList shouldEqual List.empty[Int]
  }

  "constant2 'hi'" should "generate 7 'hi's after take(7) (type test)" in {
    Stream.constant2("hi").take(7).toList shouldEqual List("hi", "hi", "hi", "hi", "hi", "hi", "hi")
  }

  "from2" should "generate 4 5 6 7 after take(4) when supplied initial value 4" in {
    Stream.from2(4).take(4).toList shouldEqual List(4, 5, 6, 7)
  }

  it should "generate -1 0 1 2 after take(4) when supplied initial value -1" in {
    Stream.from2(-1).take(4).toList shouldEqual List(-1, 0, 1, 2)
  }

  "fibs2 take 7" should "generate the first 7 elements of the Fibonacci sequence" in {
    Stream.fibs2.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  "map2 length" should "transform a stream of Strings" in {
    Stream("one", "two", "three", "four").map2(_.length).toList shouldEqual List(3, 3, 5, 4)
  }

  it should "return empty stream of type Int on empty stream" in {
    Stream.empty[String].map2(_.length).toList shouldEqual List.empty[Int]
  }

  "take2 4" should "give 4 elements when stream has more elements" in {
    Stream(1, 2, 3, 4, 5, 6, 7, 8).take2(4).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "give 3 elements when stream has fewer elements" in {
    Stream(1, 2, 3).take2(4).toList shouldEqual List(1, 2, 3)
  }

  it should "give empty stream when stream is empty" in {
    Stream.empty[Int].take2(4).toList shouldEqual List.empty[Int]
  }

  "takeWhile3 < 5" should "return the first 4 elements of Stream(1, 2, 3, 4, 5, 6)" in {
    Stream(1, 2, 3, 4, 5, 6).takeWhile3(_ < 5).toList shouldEqual List(1, 2, 3, 4)
  }

  it should "return the full stream when all elements match" in {
    Stream(1, 2, 3).takeWhile3(_ < 5).toList shouldEqual List(1, 2, 3)
  }

  it should "return empty stream when no elements match" in {
    Stream(11, 12, 13).takeWhile3(_ < 5).toList shouldEqual List.empty[Int]
  }

  it should "return empty stream on empty stream" in {
    Stream.empty[Int].takeWhile3(_ < 5).toList shouldEqual List.empty[Int]
  }

  "zipWith" should "generate correct zipped values for matched length streams" in {
    Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).take(3).toList shouldEqual List(5, 7, 9)
  }

  it should "abort after smaller first stream" in {
    Stream(1, 2).zipWith(Stream(4, 5, 6))(_ + _).take(3).toList shouldEqual List(5, 7)
  }

  it should "abort after smaller second stream" in {
    Stream(1, 2, 3).zipWith(Stream(4, 5))(_ + _).take(3).toList shouldEqual List(5, 7)
  }

  it should "zip different types" in {
    Stream("one", "two", "three")
      .zipWith(Stream(1, 2, 3))((s, i) => s + s": $i")
      .take(3)
      .toList shouldEqual List("one: 1", "two: 2", "three: 3")
  }

  "zipAll" should "generate Some Some for equal length streams" in {
    Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).take(3).toList match {
      case _ :: _ :: (Some(3), Some(6)) :: _ => assert(true)
      case _ => assert(false)
    }
  }

  it should "generate None Some for shorter first stream" in {
    Stream(1, 2).zipAll(Stream(4, 5, 6)).take(3).toList match {
      case _ :: _ :: (None, Some(6)) :: _ => assert(true)
      case _ => assert(false)
    }
  }

  it should "generate Some None for shorter second stream" in {
    Stream(1, 2, 3).zipAll(Stream(4, 5)).take(3).toList match {
      case _ :: _ :: (Some(3), None) :: _ => assert(true)
      case _ => assert(false)
    }
  }

  "startsWith Stream(1, 2, 3)" should "return true for Stream(1, 2, 3, 4, 5)" in {
    Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3)) shouldBe true
  }

  it should "return false for Stream(1, 2, 4, 5)" in {
    Stream(1, 2, 4, 5).startsWith(Stream(1, 2, 3)) shouldBe false
  }

  it should "return false for empty stream" in {
    Stream.empty[Int].startsWith(Stream(1, 2, 3)) shouldBe false
  }

  "startsWith empty[Int]" should "return true for Stream(1, 2, 3)" in {
    Stream(1, 2, 3).startsWith(Stream.empty[Int]) shouldBe true
  }

  "tails Stream(1, 2, 3)" should "return valid sub streams" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldEqual List(List(1, 2, 3), List(2, 3), List(3), Nil)
  }

  "hasSubsequence Stream(2, 3, 4)" should "return true for Stream(1, 2, 3, 4, 5)" in {
    Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3, 4)) shouldBe true
  }

  it should "return false for Stream(1, 2, 3)" in {
    Stream(1, 2, 3).hasSubsequence(Stream(2, 3, 4)) shouldBe false
  }

  it should "return true for Stream(2, 3, 4, 5)" in {
    Stream(2, 3, 4, 5).hasSubsequence(Stream(2, 3, 4)) shouldBe true
  }
}
