package problems.multiwaytree

case class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  /**
   * P70C (*) Count the nodes of a multiway tree.
   * Write a method nodeCount which counts the nodes of a given multiway tree.
   *
   * scala> MTree('a', List(MTree('f'))).nodeCount
   * res0: Int = 2
   */
  def nodeCount: Int = 1 + children.map { _.nodeCount }.sum

  /**
   * P70 (**) Tree construction from a node string.
   * Write the reverse function, and make it the toString method of MTree.
   *
   * scala> MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
   * res0: String = afg^^c^bd^e^^^
   */
  def toString2: String = ???
}

object MTree {
  def apply[A](value: A): MTree[A] = MTree(value, Nil)

  /**
   * P70 (**) Tree construction from a node string.
   * We suppose that the nodes of a multiway tree contain single characters.
   * In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal,
   * the move is a backtrack to the previous level.
   * By this rule, the tree in the figure opposite is represented as:
   *
   * afg^^c^bd^e^^^
   *
   * Define the syntax of the string and write a function string2MTree to construct an MTree from a String.
   *
   * scala> MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
   * res0: String = afg^^c^bd^e^^^
   */
  import scala.language.implicitConversions
  implicit def s2MT(s: String): MTree[Char] = string2MTree(s)
  def string2MTree(s: String): MTree[Char] = ???
}
