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

  "Tree#isSymmetric" should {
    "returns true if this = End" in {
      End.isSymmetric must beTrue
    }

    "returns true if this = Node('a)" in {
      Node('a).isSymmetric must beTrue
    }

    "returns false if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).isSymmetric must beFalse
    }

    "returns false if this = Node('a, End, Node('b))" in {
      Node('a, End, Node('b)).isSymmetric must beFalse
    }

    "returns true if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).isSymmetric must beTrue
    }

    "returns false if this = Node('a, Node('b, Node('d), End), Node('c))" in {
      Node('a, Node('b, Node('d), End), Node('c)).isSymmetric must beFalse
    }
  }

  "Tree#addValue(A)" should {
    "returns T(2 . .) if this = End" in {
      End.addValue(2) must beEqualTo(Node(2))
    }

    "returns T(2 . T(3 . .)) if this = Node(2)" in {
      Node(2).addValue(3) must beEqualTo(Node(2, End, Node(3)))
    }

    "returns T(2 T(0 . .) T(3 . .)) if this = Node(2, End, Node(3))" in {
      Node(2, End, Node(3)).addValue(0) must beEqualTo(Node(2, Node(0), Node(3)))
    }

    "returns T(2 T(0 . .) T(3 . T(4 . .))) if this = Node(2, Node(0), Node(3))" in {
      Node(2, Node(0), Node(3)).addValue(4) must beEqualTo(Node(2, Node(0), Node(3, End, Node(4))))
    }
  }

  "Tree->fromList(List[Int])" should {
    "returns T(2 . .) if list = [2]" in {
      Tree.fromList(List(2)) must beEqualTo(Node(2))
    }

    "returns T(2 . T(3 . .)) if list = [2, 3]" in {
      Tree.fromList(List(2, 3)) must beEqualTo(Node(2, End, Node(3)))
    }

    "returns T(2 T(0 . .) T(3 . .)) if list = [2, 3, 0]" in {
      Tree.fromList(List(2, 3, 0)) must beEqualTo(Node(2, Node(0), Node(3)))
    }

    "returns T(2 T(0 . .) T(3 . T(4 . .))) if list = [2, 3, 0, 4]" in {
      Tree.fromList(List(2, 3, 0, 4)) must beEqualTo(Node(2, Node(0), Node(3, End, Node(4))))
    }

    "returns T(3 T(2 T(1 . .) .) T(5 . T(7 . .))) if list = [3, 2, 5, 7, 1]" in {
      Tree.fromList(List(3, 2, 5, 7, 1)) must beEqualTo(Node(3, Node(2, Node(1), End), Node(5, End, Node(7))))
    }
  }

  "Tree->fromList(List[Int]) check" should {
    "returns true if list = [5, 3, 18, 1, 4, 12, 21]" in {
      Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric must beTrue
    }

    "returns false if list = [3, 2, 5, 7, 4]" in {
      Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric must beFalse
    }
  }
}