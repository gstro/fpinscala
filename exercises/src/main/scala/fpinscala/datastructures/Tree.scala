package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(onLeaf: A => B)(onBranch: (B, B) => B): B = t match {
    case Leaf(v) => onLeaf(v)
    case Branch(l, r) => onBranch(fold(l)(onLeaf)(onBranch), fold(r)(onLeaf)(onBranch))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def foldMax(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((b1, b2) => 1 + (b1 max b2))

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(v => Leaf(f(v)))(Branch(_, _))
}