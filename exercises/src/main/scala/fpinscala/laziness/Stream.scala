package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _          => List.empty[A]
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) => if (n > 0) t().drop(n - 1) else this
    case _          => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _                    => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](x: => Stream[B]): Stream[B] =
    foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _                    => None
    }

  def zipWith[B, C](r: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, r)) {
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _                          => None
    }

  def zipAll[B](r: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, r)) {
      case (Cons(a, as), Empty)       => Some((Some(a()), None), (as(), empty[B]))
      case (Empty, Cons(b, bs))       => Some((None, Some(b())), (empty[A], bs()))
      case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
      case _                          => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile { case (_, b) => b.isDefined }
      .forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) => Some((s, t()))
      case _              => None
    }.append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def next(i: Int, n: Int): Stream[Int] =
      cons(i, next(n, i + n))
    next(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case _           => empty
  }

  val ones2: Stream[Int] = unfold(1)(Some(1, _))

  def constant2[A](a: A): Stream[A] = unfold(a)(Some(a, _))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs2: Stream[Int] = unfold(0, 1) { case (a, b) => Some(a, (b, a + b)) }

}