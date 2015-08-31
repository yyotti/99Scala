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
}

object MTree {
  def apply[A](value: A): MTree[A] = MTree(value, Nil)
}
