package test.problems

import problems.binarytree._
import org.specs2.mutable._

class BinaryTreesSpec extends Specification {
  "Tree->cBalanced(Int, A)" should {
    "returns [.] if (n, value) = (-1, 'a)" in {
      Tree.cBalanced(0, 'a) must beEqualTo(List(End))
    }

    "returns [.] if (n, value) = (0, 'a)" in {
      Tree.cBalanced(0, 'a) must beEqualTo(List(End))
    }

    "returns [T(a . .)] if (n, value) = (1, 'a)" in {
      Tree.cBalanced(1, 'a) must beEqualTo(List(Node('a)))
    }

    "returns [T(b T(b . .) .), T(b . T(b . .))] if (n, value) = (2, 'b)" in {
      Tree.cBalanced(2, 'b) must beEqualTo(List(Node('b, Node('b), End), Node('b, End, Node('b))))
    }

    "returns [T(c T(c . .) T(c . .))] if (n, value) = (3, 'c)" in {
      Tree.cBalanced(3, 'c) must beEqualTo(List(Node('c, Node('c), Node('c))))
    }

    """returns [T("x" T("x" T("x" . .) .) T("x" . .)), T("x" T("x" . T("x" . .)) T("x" . .)), T("x" T("x" . .) T("x" T("x" . .) .)), T("x" T("x" . .) T("x" . T("x" . .)))] if (n, value) = (4, "x")""" in {
      val expected = List(
        Node("x", Node("x", Node("x"), End), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", End, Node("x")))
      )
      Tree.cBalanced(4, "x") must beEqualTo(expected)
    }
  }
}
