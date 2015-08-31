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

  def nodeCount: Int

  /**
   * P61 (*) Count the leaves of a binary tree.
   * A leaf is a node with no successors. Write a method leafCount to count them.
   *
   * scala> Node('x', Node('x'), End).leafCount
   * res0: Int = 1
   */
  def leafCount: Int

  /**
   * 61A (*) Collect the leaves of a binary tree in a list.
   * A leaf is a node with no successors. Write a method leafList to collect them in a list.
   *
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
   * res0: List[Char] = List(b, d, e)
   */
  def leafList: List[T]

  /**
   * P62 (*) Collect the internal nodes of a binary tree in a list.
   * An internal node of a binary tree has either one or two non-empty successors. Write a method internalList to collect them in a list.
   *
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
   * res0: List[Char] = List(a, c)
   */
  def internalList: List[T]

  /**
   * P62B (*) Collect the nodes at a given level in a list.
   * A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a method atLevel to collect all nodes at a given level in a list.
   *
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
   * res0: List[Char] = List(b, c)
   *
   * Using atLevel it is easy to construct a method levelOrder which creates the level-order sequence of the nodes. However, there are more efficient ways to do that.
   */
  def atLevel(level: Int): List[T]
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

  def nodeCount: Int = left.nodeCount + right.nodeCount + 1

  def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  def leafList: List[T] = (left, right) match {
    case (End, End) => List(value)
    case _ => left.leafList ::: right.leafList
  }

  def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => (value :: left.internalList) ::: right.internalList
  }

  def atLevel(level: Int): List[T] = level match {
    case level if level <= 0 => Nil
    case 1 => List(value)
    case _ => left.atLevel(level - 1) ::: right.atLevel(level - 1)
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."

  def isMirrorOf[A](tree: Tree[A]): Boolean = tree == End
  def isSymmetric: Boolean = true

  def addValue[A <% Ordered[A]](value: A): Tree[A] = Node(value)

  val nodeCount: Int = 0

  val leafCount: Int = 0

  val leafList: List[Nothing] = Nil

  val internalList: List[Nothing] = Nil

  def atLevel(level: Int): List[Nothing] = Nil
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
    else Stream.from(1).takeWhile { minHbalNodes(_) <= n }.last

  /**
   * P60 (**) Construct height-balanced binary trees with a given number of nodes.
   * Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.
   *
   * scala> Tree.hbalTreesWithNodes(4, "x")
   * res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
   *
   * Find out how many height-balanced trees exist for N = 15.
   */
  def hbalTreesWithNodes[A](n: Int, value: A): List[Tree[A]] =
    if (n <= 0) List(End)
    else if (n == 1) List(Node(value))
    else (minHbalHeight(n) to maxHbalHeight(n)).toList.flatMap { hbalTrees(_, value) }.filter { _.nodeCount == n }

  def minHbalHeight(n: Int): Int =
    if (n <= 0) 0
    else minHbalHeight(n / 2) + 1

  /**
   * P63 (**) Construct a complete binary tree.
   * A complete binary tree with height H is defined as follows:
   * The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2^(i-1) at the level i, note that we start counting the levels from 1 at the root).
   * In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
   * This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the Ends which are not really nodes!)
   * come last.
   *
   * Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
   *
   * We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1.
   * In doing so, we realize that for every node X with address A the following property holds:
   * The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist.
   * This fact can be used to elegantly construct a complete binary tree structure.
   * Write a method completeBinaryTree that takes as parameters the number of nodes and the value to put in each node.
   *
   * scala> Tree.completeBinaryTree(6, "x")
   * res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
   */
  def completeBinaryTree[A](n: Int, value: A): Tree[A] = {
    def completeBinaryTreeR(i: Int): Tree[A] =
      if (i > n) End
      else Node(value, completeBinaryTreeR(2 * i), completeBinaryTreeR(2 * i + 1))

    if (n <= 0) End
    else completeBinaryTreeR(1)
  }
}
