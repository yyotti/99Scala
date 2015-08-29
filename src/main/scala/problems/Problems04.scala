package problems.binarytree

sealed abstract class Tree[+T] {

  /**
   * P56 (**) Symmetric binary trees.
   * Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree.
   * Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric.
   *
   * Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another.
   * We are only interested in the structure, not in the contents of the nodes.
   *
   * scala> Node('a', Node('b'), Node('c')).isSymmetric
   * res0: Boolean = true
   */
  def isMirrorOf[A](tree: Tree[A]): Boolean
  def isSymmetric: Boolean

  /**
   * P57 (**) Binary search trees (dictionaries).
   * Write a function to add an element to a binary search tree.
   *
   * scala> End.addValue(2)
   * res0: Node[Int] = T(2 . .)
   *
   * scala> res0.addValue(3)
   * res1: Node[Int] = T(2 . T(3 . .))
   *
   * scala> res1.addValue(0)
   * res2: Node[Int] = T(2 T(0 . .) T(3 . .))
   *
   * Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U].
   * The >: T is because addValue's parameters need to be contravariant in T.
   * (Conceptually, we're adding nodes above existing nodes. In order for the subnodes to be of type T or any subtype,
   * the upper nodes must be of type T or any supertype.)
   * The <% Ordered[U] allows us to use the < operator on the values in the tree.
   */
  def addValue[A >: T <% Ordered[A]](value: A): Tree[A]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  def isMirrorOf[A](tree: Tree[A]): Boolean = tree match {
    case Node(_, l, r) => l.isMirrorOf(right) && r.isMirrorOf(left)
    case _ => false
  }
  def isSymmetric = left.isMirrorOf(right)

  def addValue[A >: T <% Ordered[A]](v: A): Tree[A] =
    if (v < value) copy(left = left.addValue(v))
    else copy(right = right.addValue(v))
}

case object End extends Tree[Nothing] {
  override def toString = "."

  def isMirrorOf[A](tree: Tree[A]): Boolean = tree == End
  def isSymmetric: Boolean = true

  def addValue[A <% Ordered[A]](value: A): Tree[A] = Node(value)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  // [テンプレート]
  //
  // /**
  //  * P31 (**) Determine whether a given integer number is prime.
  //  *
  //  * scala> 7.isPrime
  //  * res0: Boolean = true
  //  */
  // def isPrime: Boolean = ???

  /**
   * P55 (**) Construct completely balanced binary trees.
   * In a completely balanced binary tree, the following property holds for every node:
   * The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal,
   * which means their difference is not greater than one.
   *
   * Define an object named Tree.
   * Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes.
   * The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.
   *
   * scala> Tree.cBalanced(4, "x")
   * res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
   */
  def cBalanced[A](n: Int, value: A): List[Tree[A]] =
    if (n < 1) List(End)
    else if ((n - 1) % 2 == 0) {
      cBalanced((n - 1) / 2, value).flatMap { t1 =>
        cBalanced((n - 1) / 2, value).map { t2 =>
          Node(value, t1, t2)
        }
      }
    } else {
      val m = n / 2
      cBalanced(m, value).flatMap { t1 =>
        cBalanced(n - m - 1, value).flatMap { t2 =>
          List(Node(value, t1, t2), Node(value, t2, t1))
        }
      }
    }

  /**
   * P57 (**) Binary search trees (dictionaries).
   * Use that function to construct a binary tree from a list of integers.
   *
   * scala> Tree.fromList(List(3, 2, 5, 7, 1))
   * res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
   *
   * Finally, use that function to test your solution to P56.
   *
   * scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
   * res4: Boolean = true
   *
   * scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
   * res5: Boolean = false
   */
  def fromList(list: List[Int]): Tree[Int] = list.foldLeft(End: Tree[Int]) { case (t, v) => t.addValue(v) }

  /**
   * P58 (**) Generate-and-test paradigm.
   * Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
   *
   * scala> Tree.symmetricBalancedTrees(5, "x")
   * res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
   */
  def symmetricBalancedTrees[A](n: Int, value: A): List[Tree[A]] = cBalanced(n, value).filter { _.isSymmetric }

  /**
   * P59 (**) Construct height-balanced binary trees.
   * In a height-balanced binary tree, the following property holds for every node:
   * The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
   * Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value for the nodes.
   * The function should generate all solutions.
   *
   * scala> Tree.hbalTrees(3, "x")
   * res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
   */
  def hbalTrees[A](height: Int, value: A): List[Tree[A]] =
    if (height < 1) List(End)
    else if (height == 1) List(Node(value))
    else {
      val subtree1 = hbalTrees(height - 1, value)
      val subtree2 = hbalTrees(height - 2, value)

      val list1 = subtree1.flatMap { t1 =>
        subtree1.flatMap { t2 =>
          List(Node(value, t1, t2))
        }
      }

      val list2 = subtree1.flatMap { t1 =>
        subtree2.flatMap { t2 =>
          List(Node(value, t1, t2), Node(value, t2, t1))
        }
      }

      list1 ::: list2
    }

  /**
   * P60 (**) Construct height-balanced binary trees with a given number of nodes.
   * Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? Clearly, MaxN = 2^H - 1.
   * However, what is the minimum number MinN? This question is more difficult.
   * Try to find a recursive statement and turn it into a function minHbalNodes that takes a height and returns MinN.
   *
   * scala> minHbalNodes(3)
   * res0: Int = 4
   */
  def minHbalNodes(height: Int): Int =
    if (height <= 0) 0
    else minHbalNodes(height - 1) + minHbalNodes(height - 2) + 1

  /**
   * P60 (**) Construct height-balanced binary trees with a given number of nodes.
   * On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a maxHbalHeight function.
   *
   * scala> maxHbalHeight(4)
   * res1: Int = 3
   */
  def maxHbalHeight(n: Int): Int =
    if (n <= 0) 0
    else maxHbalHeight(n / 2) + 1

  /**
   * P60 (**) Construct height-balanced binary trees with a given number of nodes.
   * Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.
   *
   * scala> Tree.hbalTreesWithNodes(4, "x")
   * res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
   *
   * Find out how many height-balanced trees exist for N = 15.
   */
  def hbalTreesWithNodes[A](n: Int, value: A): List[Tree[A]] = ???
}
