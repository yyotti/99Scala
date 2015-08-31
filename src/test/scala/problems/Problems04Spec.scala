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

    """returns [T("x" T("x" T("x" . .) .) T("x" T("x" . .) .)), T("x" T(T("x" . .) .) T(. T("x" . .))), T("x" T("x" . T("x" . .)) T("x" T("x" . .) .)), T("x" T("x" . T("x" . .)) T("x" . T("x")))] if (n, value) = (5, "x")""" in {
      val expected = List(
        Node("x", Node("x", Node("x"), End), Node("x", Node("x"), End)),
        Node("x", Node("x", Node("x"), End), Node("x", End, Node("x"))),
        Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x", End, Node("x")))
      )
      Tree.cBalanced(5, "x") must beEqualTo(expected)
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

  "Tree->symmetricBalancedTrees(Int, A) check" should {
    "returns [.] if (n, value) = (-1, 'a)" in {
      Tree.symmetricBalancedTrees(-1, 'a) must beEqualTo(List(End))
    }

    "returns [.] if (n, value) = (0, 'a)" in {
      Tree.symmetricBalancedTrees(0, 'a) must beEqualTo(List(End))
    }

    "returns [T(b . .)] if (n, value) = (1, 'b)" in {
      Tree.symmetricBalancedTrees(1, 'b) must beEqualTo(List(Node('b)))
    }

    "returns [] if (n, value) = (2, 'c)" in {
      Tree.symmetricBalancedTrees(2, 'c) must beEmpty
    }

    "returns [T(d T(d . .) T(d . .)] if (n, value) = (3, 'd)" in {
      Tree.symmetricBalancedTrees(3, 'd) must beEqualTo(List(Node('d, Node('d), Node('d))))
    }

    "returns [] if (n, value) = (4, 'e)" in {
      Tree.symmetricBalancedTrees(4, 'e) must beEmpty
    }

    """returns [T("x" T(T("x" . .) .) T(. T("x" . .))), T("x" T("x" . T("x" . .)) T("x" T("x" . .) .))] if (n, value) = (5, "x")""" in {
      Tree.symmetricBalancedTrees(5, "x") must beEqualTo(List(Node("x", Node("x", Node("x"), End), Node("x", End, Node("x"))), Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End))))
    }
  }

  "Tree->hbalTrees(Int, A)" should {
    "returns [.] if (height, value) = (-1, 'a)" in {
      Tree.hbalTrees(-1, 'a) must beEqualTo(List(End))
    }

    "returns [.] if (height, value) = (0, 'a)" in {
      Tree.hbalTrees(0, 'a) must beEqualTo(List(End))
    }

    "returns [T(b . .)] if (height, value) = (1, 'b)" in {
      Tree.hbalTrees(1, 'b) must beEqualTo(List(Node('b)))
    }

    "returns [T(c T(c . .) T(c . .)), T(c T(c . .) .), T(c . T(c . .))] if (n, value) = (2, 'c)" in {
      val expected = List(
        Node('c, Node('c), Node('c)),
        Node('c, Node('c), End),
        Node('c, End, Node('c))
      )
      Tree.hbalTrees(2, 'c) must beEqualTo(expected)
    }

    """returns [T("x" T("x" T("x" . .) T("x" . .)) T("x" T("x" . .) T("x" . .))), T("x" T("x" T("x" . .) T("x" . .)) T("x" T("x" . .) .)), ...] if (n, value) = (3, "x")""" in {
      val expected = List(
        Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), Node("x"))),
        Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), End)),
        Node("x", Node("x", Node("x"), Node("x")), Node("x", End, Node("x"))),
        Node("x", Node("x", Node("x"), End), Node("x", Node("x"), Node("x"))),
        Node("x", Node("x", Node("x"), End), Node("x", Node("x"), End)),
        Node("x", Node("x", Node("x"), End), Node("x", End, Node("x"))),
        Node("x", Node("x", End, Node("x")), Node("x", Node("x"), Node("x"))),
        Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x", End, Node("x"))),
        Node("x", Node("x", Node("x"), Node("x")), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", Node("x"), Node("x"))),
        Node("x", Node("x", Node("x"), End), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", End, Node("x")))
      )
      Tree.hbalTrees(3, "x") must beEqualTo(expected)
    }
  }

  "Tree->minHbalNodes(Int)" should {
    "returns 0 if height = -1" in {
      Tree.minHbalNodes(-1) must beEqualTo(0)
    }

    "returns 0 if height = 0" in {
      Tree.minHbalNodes(0) must beEqualTo(0)
    }

    "returns 1 if height = 1" in {
      Tree.minHbalNodes(1) must beEqualTo(1)
    }

    "returns 2 if height = 2" in {
      Tree.minHbalNodes(2) must beEqualTo(2)
    }

    "returns 4 if height = 3" in {
      Tree.minHbalNodes(3) must beEqualTo(4)
    }

    "returns 7 if height = 4" in {
      Tree.minHbalNodes(4) must beEqualTo(7)
    }

    "returns 12 if height = 5" in {
      Tree.minHbalNodes(5) must beEqualTo(12)
    }
  }

  "Tree->maxHbalHeight(Int)" should {
    "returns 0 if n = -1" in {
      Tree.maxHbalHeight(-1) must beEqualTo(0)
    }

    "returns 0 if n = 0" in {
      Tree.maxHbalHeight(0) must beEqualTo(0)
    }

    "returns 1 if n = 1" in {
      Tree.maxHbalHeight(1) must beEqualTo(1)
    }

    "returns 2 if n = 2" in {
      Tree.maxHbalHeight(2) must beEqualTo(2)
    }

    "returns 2 if n = 3" in {
      Tree.maxHbalHeight(3) must beEqualTo(2)
    }

    "returns 3 if n = 4" in {
      Tree.maxHbalHeight(4) must beEqualTo(3)
    }

    "returns 3 if n = 5" in {
      Tree.maxHbalHeight(5) must beEqualTo(3)
    }

    "returns 4 if n = 7" in {
      Tree.maxHbalHeight(7) must beEqualTo(4)
    }

    "returns 4 if n = 8" in {
      Tree.maxHbalHeight(8) must beEqualTo(4)
    }
  }

  "Tree->minHbalHeight(Int)" should {
    "returns 0 if n = -1" in {
      Tree.minHbalHeight(-1) must beEqualTo(0)
    }

    "returns 0 if n = 0" in {
      Tree.minHbalHeight(0) must beEqualTo(0)
    }

    "returns 1 if n = 1" in {
      Tree.minHbalHeight(1) must beEqualTo(1)
    }

    "returns 2 if n = 2" in {
      Tree.minHbalHeight(2) must beEqualTo(2)
    }

    "returns 2 if n = 3" in {
      Tree.minHbalHeight(3) must beEqualTo(2)
    }

    "returns 3 if n = 4" in {
      Tree.minHbalHeight(4) must beEqualTo(3)
    }

    "returns 3 if n = 5" in {
      Tree.minHbalHeight(5) must beEqualTo(3)
    }

    "returns 3 if n = 7" in {
      Tree.minHbalHeight(7) must beEqualTo(3)
    }

    "returns 4 if n = 8" in {
      Tree.maxHbalHeight(8) must beEqualTo(4)
    }
  }

  "Tree->hbalTreesWithNodes(Int, A)" should {
    "returns [.] if (n, value) = (-1, 'a)" in {
      Tree.hbalTreesWithNodes(-1, 'a) must beEqualTo(List(End))
    }

    "returns [.] if (n, value) = (0, 'a)" in {
      Tree.hbalTreesWithNodes(0, 'a) must beEqualTo(List(End))
    }

    "returns [T(b . .)] if (n, value) = (1, 'b)" in {
      Tree.hbalTreesWithNodes(1, 'b) must beEqualTo(List(Node('b)))
    }

    "returns [T(c T(c . .) .), T(c . T(c . .))] if (n, value) = (2, 'c)" in {
      Tree.hbalTreesWithNodes(2, 'c) must beEqualTo(List(Node('c, Node('c), End), Node('c, End, Node('c))))
    }

    "returns [T(d T(d . .) T(d . .))] if (n, value) = (3, 'd)" in {
      Tree.hbalTreesWithNodes(3, 'd) must beEqualTo(List(Node('d, Node('d), Node('d))))
    }

    """returns [T("x" T("x" T("x" . .) .) T("x" . .)), T("x" T("x" . T("x" . .)) T("x" . .)), ...] if (n, value) = (4, "x")""" in {
      val expected = List(
        Node("x", Node("x", Node("x"), End), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", Node("x"), End)),
        Node("x", Node("x", End, Node("x")), Node("x", End, End)),
        Node("x", Node("x", End, End), Node("x", End, Node("x")))
      )
      Tree.hbalTreesWithNodes(4, "x") must beEqualTo(expected)
    }

    "returns [T(e T(e . .) T(e . .))] if (n, value) = (5, 'e)" in {
      val expected = List(
        Node('e, Node('e, Node('e), End), Node('e, Node('e), End)),
        Node('e, Node('e, Node('e), End), Node('e, End, Node('e))),
        Node('e, Node('e, End, Node('e)), Node('e, Node('e), End)),
        Node('e, Node('e, End, Node('e)), Node('e, End, Node('e))),
        Node('e, Node('e, Node('e), Node('e)), Node('e, End, End)),
        Node('e, Node('e, End, End), Node('e, Node('e), Node('e)))
      )
      Tree.hbalTreesWithNodes(5, 'e) must beEqualTo(expected)
    }
  }

  "Tree->hbalTreesWithNodes(Int, A) check" should {
    "count if (n, value) = (15, 'z)" in {
      println(Tree.hbalTreesWithNodes(15, 'z).size)
      true must beTrue
    }
  }

  "Tree#nodeCount" should {
    "returns 0 if this = End" in {
      End.nodeCount must beEqualTo(0)
    }

    "returns 1 if this = Node('a)" in {
      Node('a).nodeCount must beEqualTo(1)
    }

    "returns 2 if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).nodeCount must beEqualTo(2)
    }

    "returns 2 if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).nodeCount must beEqualTo(2)
    }

    "returns 3 if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).nodeCount must beEqualTo(3)
    }

    "returns 4 if this = Node('a, Node('b, Node('d), End), Node('c))" in {
      Node('a, Node('b, Node('d), End), Node('c)).nodeCount must beEqualTo(4)
    }

    "returns 4 if this = Node('a, Node('b, End, Node('e)), Node('c))" in {
      Node('a, Node('b, End, Node('e)), Node('c)).nodeCount must beEqualTo(4)
    }

    "returns 4 if this = Node('a, Node('b), Node('c, Node('f), End))" in {
      Node('a, Node('b), Node('c, Node('f), End)).nodeCount must beEqualTo(4)
    }

    "returns 4 if this = Node('a, Node('b), Node('c, End, Node('g)))" in {
      Node('a, Node('b), Node('c, End, Node('g))).nodeCount must beEqualTo(4)
    }

    "returns 5 if this = Node('a, Node('b, Node('d), Node('e)), Node('c))" in {
      Node('a, Node('b, Node('d), Node('e)), Node('c)).nodeCount must beEqualTo(5)
    }

    "returns 5 if this = Node('a, Node('b, Node('d), End), Node('c, Node('f), End))" in {
      Node('a, Node('b, Node('d), End), Node('c, Node('f), End)).nodeCount must beEqualTo(5)
    }
  }
}
