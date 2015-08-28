package problems.binarytree

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
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
      cBalanced((n - 1) / 2, value).map { t =>
        Node(value, t, t)
      }
    } else {
      val m = n / 2
      cBalanced(m, value).flatMap { t1 =>
        cBalanced(n - m - 1, value).flatMap { t2 =>
          List(Node(value, t1, t2), Node(value, t2, t1))
        }
      }
    }
}
