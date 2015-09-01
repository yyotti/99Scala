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
  def toString2: String = value.toString + children.map { _.toString2 }.mkString + "^"
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
  import util.parsing.combinator._
  def string2MTree(s: String): MTree[Char] = TreeStringParser.parse(s) match {
    case TreeStringParser.Success(t, _) => t
    case _ => throw new IllegalArgumentException
  }
  object TreeStringParser extends RegexParsers {
    def back = "^"
    def value: Parser[Char] = "[a-z]".r ^^ { c => c.head }
    def node: Parser[MTree[Char]] = value ~ rep(node) ~ back ^^ { case v ~ children ~ _ => MTree(v, children) }
    def parse(input: String) = parseAll(node, input)
  }
}
