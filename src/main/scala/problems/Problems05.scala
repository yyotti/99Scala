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

  /**
   * P71 (*) Determine the internal path length of a tree.
   * We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree.
   * By this definition, the tree in the figure of problem P70 has an internal path length of 9.
   * Write a method internalPathLength to return that sum.
   *
   * scala> "afg^^c^bd^e^^^".internalPathLength
   * res0: Int = 9
   */
  def internalPathLength: Int = children.foldLeft(0) { case (sum, c) => sum + c.nodeCount + c.internalPathLength }

  /**
   * P72 (*) Construct the postorder sequence of the tree nodes.
   * Write a method postorder which constructs the postorder sequence of the nodes of a multiway tree. The result should be a List.
   *
   * scala> "afg^^c^bd^e^^^".postorder
   * res0: List[Char] = List(g, f, c, d, e, b, a)
   */
  def postorder: List[T] = children.flatMap { _.postorder } ::: List(value)

  /**
   * P73 (**) Lisp-like tree representation.
   * There is a particular notation for multiway trees in Lisp.
   * Lisp is a prominent functional programming language. In Lisp almost everything is a list.
   *
   * Our example tree would be represented in Lisp as (a (f g) c (b d e)). The following pictures give some more examples.
   *
   * Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list,
   * followed by its children.
   * The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', with the atoms separated by spaces.
   * We can represent this syntax as a Scala String. Write a method lispyTree which constructs a "lispy string" from an MTree.
   *
   * scala> MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree
   * res0: String = (a (b c))
   */
  def lispyTree: String = children match {
    case Nil => value.toString
    case _ => "(" + (value.toString :: children.map { _.lispyTree}).mkString(" ") + ")"
  }
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

  /**
   * P73 (**) Lisp-like tree representation.
   * As a second, even more interesting, exercise try to write a method that takes a "lispy" string and turns it into a multiway tree.
   */
  def fromLispyString(s: String): MTree[Char] = ???
}
