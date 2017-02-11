import fpinscala.datastructures._

val tree: Tree[Int] = Branch(Branch(Leaf(3), Branch(Leaf(7), Branch(Branch(Leaf(3), Branch(Leaf(3), Branch(Leaf(3), Branch(Leaf(3), Branch(Leaf(3), Leaf(4)))))), Leaf(4)))), Leaf(8))

Tree.size(tree) == Tree.foldSize(tree)
Tree.maximum(tree) == Tree.foldMax(tree)
Tree.depth(tree) == Tree.foldDepth(tree)
Tree.map(tree)(_.toDouble) == Tree.foldMap(tree)(_.toDouble)
