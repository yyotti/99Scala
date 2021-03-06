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

  /**
   * P64 (**) Layout a binary tree (1).
   * As a preparation for drawing a tree, a layout algorithm is required to determine the position of each node in a rectangular grid.
   * Several layout methods are conceivable, one of them is shown in the illustration on the right.
   *
   * In this layout strategy, the position of a node v is obtained by the following two rules:
   *   x(v) is equal to the position of the node v in the inorder sequence
   *   y(v) is equal to the depth of the node v in the tree
   *
   * In order to store the position of the nodes, we add a new class with the additional information.
   *
   * case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
   *   override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
   * }
   *
   * Write a method layoutBinaryTree that turns a tree of normal Nodes into a tree of PositionedNodes.
   *
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
   * res0: PositionedNode[Char] = T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))
   *
   * The tree at right may be constructed with Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')). Use it to check your code.
   */
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeSub(1, 1)._1
  def layoutBinaryTreeSub(x: Int, y: Int): (Tree[T], Int)

  /**
   * P65 (**) Layout a binary tree (2).
   * An alternative layout method is depicted in the illustration opposite.
   * Find out the rules and write the corresponding method.
   * Hint: On a given level, the horizontal distance between neighboring nodes is constant.
   *
   * Use the same conventions as in problem P64.
   *
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2
   * res0: PositionedNode[Char] = T[3,1]('a T[1,2]('b . T[2,3]('c . .)) T[5,2]('d . .))
   *
   * The tree at right may be constructed with Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')). Use it to check your code.
   */
  def layoutBinaryTree2: Tree[T] = {
    val d = treeDepth
    val x0 = (2 to leftmostNodeDepth).map { n => math.pow(2, d - n).toInt }.sum + 1
    layoutBinaryTree2Internal(x0, 1, d - 2)
  }
  def treeDepth: Int
  def leftmostNodeDepth: Int
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]

  /**
   * P66 (***) Layout a binary tree (3).
   * Yet another layout strategy is shown in the illustration opposite.
   * The method yields a very compact layout while maintaining a certain symmetry in every node.
   * Find out the rules and write the corresponding method.
   * Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?
   *
   * Use the same conventions as in problem P64 and P65.
   * Note: This is a difficult problem. Don't give up too early!
   *
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3
   * res0: PositionedNode[Char] = T[2,1]('a T[1,2]('b . T[2,3]('c . .)) T[3,2]('d . .))
   *
   * Which layout do you like most?
   */
  def layoutBinaryTree3: Tree[T] = ???

  /**
   * P67 (**) A string representation of binary trees.
   * Somebody represents binary trees as strings of the following type (see example opposite):
   * a(b(d,e),c(,f(g,)))
   * Write a method which generates this string representation, if the tree is given as usual (in Nodes and Ends).
   * Use that method for the Tree class's and subclass's toString methods.
   *
   * scala> Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString
   * res0: String = a(b(d,e),c(,f(g,)))
   */
  def toString2: String

  /**
   * P68 (**) Preorder and inorder sequences of binary trees.
   * We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
   *
   * a) Write methods preorder and inorder that construct the preorder and inorder sequence of a given binary tree, respectively.
   * The results should be lists, e.g. List('a','b','d','e','c','f','g') for the preorder sequence of the example in problem P67.
   *
   * scala> Tree.string2Tree("a(b(d,e),c(,f(g,)))").preorder
   * res0: List[Char] = List(a, b, d, e, c, f, g)
   *
   * scala> Tree.string2Tree("a(b(d,e),c(,f(g,)))").inorder
   * res1: List[Char] = List(d, b, e, a, c, g, f)
   */
  def preorder: List[T]
  def inorder: List[T]

  /**
   * P69 (**) Dotstring representation of binary trees.
   * We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
   * Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted
   * where an empty subtree (End) is encountered during the tree traversal.
   * For example, the tree shown in problem P67 is represented as "abd..e..c.fg...".
   * First, try to establish a syntax (BNF or syntax diagrams) and then write two methods, toDotstring and fromDotstring, which do the conversion in both directions.
   *
   * scala> Tree.string2Tree("a(b(d,e),c(,f(g,)))").toDotstring
   * res0: String = abd..e..c.fg...
   */
  def toDotstring: String
}

trait TreeNode[+T] extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  val value: T
  val left: Tree[T]
  val right: Tree[T]

  def isMirrorOf[A](tree: Tree[A]): Boolean = tree match {
    case Node(_, l, r) => l.isMirrorOf(right) && r.isMirrorOf(left)
    case _ => false
  }
  def isSymmetric = left.isMirrorOf(right)

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

  def layoutBinaryTreeSub(x: Int, y: Int): (Tree[T], Int) = {
    val (leftTree, thisX) = left.layoutBinaryTreeSub(x, y + 1)
    val (rightTree, nextX) = right.layoutBinaryTreeSub(thisX + 1, y + 1)

    (PositionedNode(value, leftTree, rightTree, thisX, y), nextX)
  }

  def treeDepth: Int = left.treeDepth.max(right.treeDepth) + 1
  def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T] =
    PositionedNode(
      value,
      left.layoutBinaryTree2Internal(x - math.pow(2, exp).toInt, depth + 1, exp - 1),
      right.layoutBinaryTree2Internal(x + math.pow(2, exp).toInt, depth + 1, exp - 1),
      x, depth
    )

  def toString2: String = (left, right) match {
    case (End, End) => value.toString
    case _ => s"${value.toString}(${left.toString2},${right.toString2})"
  }

  def preorder: List[T] = value :: left.preorder ::: right.preorder
  def inorder: List[T] = left.inorder ::: List(value) ::: right.inorder

  def toDotstring: String = value + left.toDotstring + right.toDotstring
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends TreeNode[T] {
  def addValue[A >: T <% Ordered[A]](v: A): Tree[A] =
    if (v < value) copy(left = left.addValue(v))
    else copy(right = right.addValue(v))
}

case class PositionedNode[+T](val value: T, val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends TreeNode[T] {
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"

  def addValue[A >: T <% Ordered[A]](v: A): Tree[A] =
    if (v < value) copy(left = left.addValue(v))
    else copy(right = right.addValue(v))
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

  def layoutBinaryTreeSub(x: Int, y: Int): (Tree[Nothing], Int) = (End, x)

  val treeDepth: Int = 0
  val leftmostNodeDepth: Int = 0
  def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int) = End

  val toString2: String = ""

  val preorder = Nil
  val inorder = Nil

  val toDotstring: String = "."
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
  def fromList[A <% Ordered[A]](list: List[A]): Tree[A] = list.foldLeft(End: Tree[A]) { case (t, v) => t.addValue(v) }

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

  /**
   * P67 (**) A string representation of binary trees.
   * Then write a method (on the Tree object) which does this inverse; i.e. given the string representation, construct the tree in the usual form.
   *
   * For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string.
   *
   * scala> Tree.fromString("a(b(d,e),c(,f(g,)))")
   * res1: Node[Char] = a(b(d,e),c(,f(g,)))
   */
  import util.parsing.combinator._
  def fromString(s: String): Tree[Char] = TreeStringParser.parse(s) match {
    case TreeStringParser.Success(t, _) => t
    case _ => End
  }
  object TreeStringParser extends RegexParsers {
    def value = "[a-z]?".r
    def node: Parser[Tree[Char]] = value ~ "(" ~ node ~ "," ~ node ~ ")" ^^ { case v ~ _ ~ left ~ _ ~ right ~ _ => Node(v.head, left, right) } | value ^^ { v => if (v.isEmpty) End else Node(v.head) }
    def parse(input: String) = parseAll(node, input)
  }

  /**
   * P68 (**) Preorder and inorder sequences of binary trees.
   * b) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously.
   * Write a method preInTree that does the job.
   *
   * scala> Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
   * res2: Node[Char] = a(b(d,e),c(,f(g,)))
   *
   * What happens if the same character appears in more than one node? Try, for instance, Tree.preInTree(List('a', 'b', 'a'), List('b', 'a', 'a')).
   */
  def preInTree[A](preList: List[A], inList: List[A]): Tree[A] = (preList, inList) match {
    case (p, i) if p.size != i.size => End
    case (Nil, Nil) => End
    case (x :: tail, _) =>
      val (inLeft, inRight) = inList.span { _ != x }
      val (preLeft, preRight) = tail.splitAt(inLeft.size)
      Node(x, preInTree(preLeft, inLeft), preInTree(preRight, inRight.tail))
  }

  /**
   * P69 (**) Dotstring representation of binary trees.
   *
   * scala> Tree.fromDotstring("abd..e..c.fg...")
   * res1: Node[Char] = a(b(d,e),c(,f(g,)))
   */
  def fromDotstring(s: String): Tree[Char] = DotstringParser.parse(s) match {
    case DotstringParser.Success(t, _) => t
    case _ => End
  }
  object DotstringParser extends RegexParsers {
    def end: Parser[Tree[Nothing]] = "[.]".r ^^ { _ => End }
    def value: Parser[Char] = "[a-z]".r ^^ { c => c.head }
    def node: Parser[Tree[Char]] = end | value ~ node ~ node ^^ { case v ~ left ~ right => Node(v, left, right) }
    def parse(input: String) = parseAll(node, input)
  }
}
