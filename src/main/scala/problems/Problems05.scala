package problems.multiwaytree

case class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
}

object MTree {
  def apply[A](value: A): MTree[A] = MTree(value, Nil)
}
