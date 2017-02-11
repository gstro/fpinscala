package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, l)

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, c) => c + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumLeft[A](l: List[Int], z: Int): Int =
    foldLeft(l, z)(_ + _)

  def productLeft[A](l: List[Int], z: Int): Int =
    foldLeft(l, z)(_ * _)

  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((c, _) => c + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((a, b) => Cons(b, a))

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a,b))

  def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def appendViaFold[A](l: List[A], rs: List[A]): List[A] =
    foldLeft(reverse(l), rs)((acc, a) => Cons(a, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def dToS(l: List[Int]): List[String] =
    foldRight(l, List[String]())((a, b) => Cons(s"$a", b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def merge(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, merge(as, bs))
  }

  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = (l, r) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def subseq(msup: List[A], msub: List[A]): Boolean = (msup, msub) match {
      case (_, Nil) => true
      case (Cons(a, as), Cons(b, bs)) if a == b => subseq(as, bs)
      case (Cons(_, as), _) => subseq(as, sub)
      case (Nil, _) => false
    }
    subseq(sup, sub)
  }

}
