package problems.binarytree

// [テンプレート]
//
// /**
//  * P31 (**) Determine whether a given integer number is prime.
//  *
//  * scala> 7.isPrime
//  * res0: Boolean = true
//  */
// def isPrime: Boolean = ???

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
