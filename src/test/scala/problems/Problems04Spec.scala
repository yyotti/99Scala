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

  "Tree#leafCount" should {
    "returns 0 if this = End" in {
      End.leafCount must beEqualTo(0)
    }

    "returns 1 if this = Node('a)" in {
      Node('a).leafCount must beEqualTo(1)
    }

    "returns 1 if this = Node('x', Node('x'), End)" in {
      Node('x', Node('x'), End).leafCount must beEqualTo(1)
    }

    "returns 1 if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).leafCount must beEqualTo(1)
    }

    "returns 2 if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).leafCount must beEqualTo(2)
    }

    "returns 2 if this = Node('a, Node('b, Node('d), End), Node('c))" in {
      Node('a, Node('b, Node('d), End), Node('c)).leafCount must beEqualTo(2)
    }

    "returns 2 if this = Node('a, Node('b, End, Node('e)), Node('c))" in {
      Node('a, Node('b, End, Node('e)), Node('c)).leafCount must beEqualTo(2)
    }

    "returns 2 if this = Node('a, Node('b), Node('c, Node('f), End))" in {
      Node('a, Node('b), Node('c, Node('f), End)).leafCount must beEqualTo(2)
    }

    "returns 2 if this = Node('a, Node('b), Node('c, End, Node('g)))" in {
      Node('a, Node('b), Node('c, End, Node('g))).leafCount must beEqualTo(2)
    }

    "returns 3 if this = Node('a, Node('b, Node('d), Node('e)), Node('c))" in {
      Node('a, Node('b, Node('d), Node('e)), Node('c)).leafCount must beEqualTo(3)
    }

    "returns 2 if this = Node('a, Node('b, Node('d), End), Node('c, Node('f), End))" in {
      Node('a, Node('b, Node('d), End), Node('c, Node('f), End)).leafCount must beEqualTo(2)
    }
  }

  "Tree#leafList" should {
    "returns [] if this = End" in {
      End.leafList must beEmpty
    }

    "returns [a] if this = Node('a)" in {
      Node('a).leafList must beEqualTo(List('a))
    }

    "returns ['x'] if this = Node('x', Node('x'), End)" in {
      Node('x', Node('x'), End).leafList must beEqualTo(List('x'))
    }

    "returns [c] if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).leafList must beEqualTo(List('c))
    }

    "returns [b, c] if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).leafList must beEqualTo(List('b, 'c))
    }

    "returns [d, c] if this = Node('a, Node('b, Node('d), End), Node('c))" in {
      Node('a, Node('b, Node('d), End), Node('c)).leafList must beEqualTo(List('d, 'c))
    }

    "returns [e, c] if this = Node('a, Node('b, End, Node('e)), Node('c))" in {
      Node('a, Node('b, End, Node('e)), Node('c)).leafList must beEqualTo(List('e, 'c))
    }

    "returns [b, f] if this = Node('a, Node('b), Node('c, Node('f), End))" in {
      Node('a, Node('b), Node('c, Node('f), End)).leafList must beEqualTo(List('b, 'f))
    }

    "returns [b, g] if this = Node('a, Node('b), Node('c, End, Node('g)))" in {
      Node('a, Node('b), Node('c, End, Node('g))).leafList must beEqualTo(List('b, 'g))
    }

    "returns [d, e, c] if this = Node('a, Node('b, Node('d), Node('e)), Node('c))" in {
      Node('a, Node('b, Node('d), Node('e)), Node('c)).leafList must beEqualTo(List('d, 'e, 'c))
    }

    "returns [d, f] if this = Node('a, Node('b, Node('d), End), Node('c, Node('f), End))" in {
      Node('a, Node('b, Node('d), End), Node('c, Node('f), End)).leafList must beEqualTo(List('d, 'f))
    }

    "returns [b, d, e] if this = Node('a, Node('b), Node('c, Node('d), Node('e)))" in {
      Node('a, Node('b), Node('c, Node('d), Node('e))).leafList must beEqualTo(List('b, 'd, 'e))
    }
  }

  "Tree#internalList" should {
    "returns [] if this = End" in {
      End.internalList must beEmpty
    }

    "returns [] if this = Node('a)" in {
      Node('a).internalList must beEmpty
    }

    "returns ['x'] if this = Node('x', Node('x'), End)" in {
      Node('x', Node('x'), End).internalList must beEqualTo(List('x'))
    }

    "returns ['a] if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).internalList must beEqualTo(List('a))
    }

    "returns ['a] if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).internalList must beEqualTo(List('a))
    }

    "returns ['a, 'b] if this = Node('a, Node('b, Node('d), End), Node('c))" in {
      Node('a, Node('b, Node('d), End), Node('c)).internalList must beEqualTo(List('a, 'b))
    }

    "returns ['a, 'b] if this = Node('a, Node('b, End, Node('e)), Node('c))" in {
      Node('a, Node('b, End, Node('e)), Node('c)).internalList must beEqualTo(List('a, 'b))
    }

    "returns ['a, 'c] if this = Node('a, Node('b), Node('c, Node('f), End))" in {
      Node('a, Node('b), Node('c, Node('f), End)).internalList must beEqualTo(List('a, 'c))
    }

    "returns ['a, 'c] if this = Node('a, Node('b), Node('c, End, Node('g)))" in {
      Node('a, Node('b), Node('c, End, Node('g))).internalList must beEqualTo(List('a, 'c))
    }

    "returns ['a, 'b] if this = Node('a, Node('b, Node('d), Node('e)), Node('c))" in {
      Node('a, Node('b, Node('d), Node('e)), Node('c)).internalList must beEqualTo(List('a, 'b))
    }

    "returns ['a, 'b, 'c] if this = Node('a, Node('b, Node('d), End), Node('c, Node('f), End))" in {
      Node('a, Node('b, Node('d), End), Node('c, Node('f), End)).internalList must beEqualTo(List('a, 'b, 'c))
    }

    "returns ['a', 'c'] if this = Node('a', Node('b'), Node('c', Node('d'), Node('e')))" in {
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList must beEqualTo(List('a', 'c'))
    }
  }

  "Tree#atLevel(Int)" should {
    "returns [] if (this, level) = (End, 2)" in {
      End.atLevel(2) must beEmpty
    }

    "returns [] if (this, level) = (Node('a), -1)" in {
      Node('a).atLevel(-1) must beEmpty
    }

    "returns [] if (this, level) = (Node('a), 0)" in {
      Node('a).atLevel(0) must beEmpty
    }

    "returns ['a] if (this, level) = (Node('a), 1)" in {
      Node('a).atLevel(1) must beEqualTo(List('a))
    }

    "returns ['a] if (this, level) = (Node('a), 2)" in {
      Node('a).atLevel(2) must beEmpty
    }

    "returns ['x'] if (this, level) = (Node('x', Node('x'), End), 1)" in {
      Node('x', Node('x'), End).atLevel(1) must beEqualTo(List('x'))
    }

    "returns ['c] if (this, level) = (Node('a, End, Node('c)), 2)" in {
      Node('a, End, Node('c)).atLevel(2) must beEqualTo(List('c))
    }

    "returns ['b, 'c] if (this, level) = (Node('a, Node('b), Node('c)), 2)" in {
      Node('a, Node('b), Node('c)).atLevel(2) must beEqualTo(List('b, 'c))
    }

    "returns ['b, 'c] if (this, level) = (Node('a, Node('b, Node('d), End), Node('c)), 2)" in {
      Node('a, Node('b, Node('d), End), Node('c)).atLevel(2) must beEqualTo(List('b, 'c))
    }

    "returns ['d] if (this, level) = (Node('a, Node('b, Node('d), End), Node('c)), 3)" in {
      Node('a, Node('b, Node('d), End), Node('c)).atLevel(3) must beEqualTo(List('d))
    }

    "returns ['b', 'c'] if (this, level) = Node('a', Node('b'), Node('c', Node('d'), Node('e')))" in {
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) must beEqualTo(List('b', 'c'))
    }
  }

  "Tree->completeBinaryTree(Int, A)" should {
    "returns End if (n, value) = (-1, 'a)" in {
      Tree.completeBinaryTree(-1, 'a) must beEqualTo(End)
    }

    "returns End if (n, value) = (0, 'a)" in {
      Tree.completeBinaryTree(0, 'a) must beEqualTo(End)
    }

    "returns T(a . .) if (n, value) = (1, 'a)" in {
      Tree.completeBinaryTree(1, 'a) must beEqualTo(Node('a))
    }

    "returns T(a T(a . .) .) if (n, value) = (2, 'a)" in {
      Tree.completeBinaryTree(2, 'a) must beEqualTo(Node('a, Node('a), End))
    }

    "returns T(a T(a . .) T(a . .)) if (n, value) = (3, 'a)" in {
      Tree.completeBinaryTree(3, 'a) must beEqualTo(Node('a, Node('a), Node('a)))
    }

    "returns T(a T(a T(a . .) .) T(a . .)) if (n, value) = (4, 'a)" in {
      Tree.completeBinaryTree(4, 'a) must beEqualTo(Node('a, Node('a, Node('a), End), Node('a)))
    }

    "returns T(a T(a T(a . .) T(a . .)) T(a . .)) if (n, value) = (5, 'a)" in {
      Tree.completeBinaryTree(5, 'a) must beEqualTo(Node('a, Node('a, Node('a), Node('a)), Node('a)))
    }

    """returns T("x" T("x" T("x" . .) T("x" . .)) T("x" T("x" . .) .)) if (n, value) = (6, "x")""" in {
      Tree.completeBinaryTree(6, "x") must beEqualTo(Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), End)))
    }

    "returns T(a T(a T(a . .) T(a . .)) T(a T(a . .) T(a . .))) if (n, value) = (7, 'a)" in {
      Tree.completeBinaryTree(7, 'a) must beEqualTo(Node('a, Node('a, Node('a), Node('a)), Node('a, Node('a), Node('a))))
    }
  }

  "Tree#layoutBinaryTree" should {
    "returns End if this = End" in {
      End.layoutBinaryTree must beEqualTo(End)
    }

    "returns T[1,1](a . .) if this = Node('a)" in {
      Node('a).layoutBinaryTree must beEqualTo(PositionedNode('a, End, End, 1, 1))
    }

    "returns T[2,1](a T[1,2](b . .) .) if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).layoutBinaryTree must beEqualTo(PositionedNode('a, PositionedNode('b, End, End, 1, 2), End, 2, 1))
    }

    "returns T[1,1](a . T[2,2](c . .)) if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).layoutBinaryTree must beEqualTo(PositionedNode('a, End, PositionedNode('c, End, End, 2, 2), 1, 1))
    }

    "returns T[2,1](a T[1,2](b . .) T[3,2](c . .)) if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).layoutBinaryTree must beEqualTo(PositionedNode('a, PositionedNode('b, End, End, 1, 2), PositionedNode('c, End, End, 3, 2), 2, 1))
    }

    "returns T[3,1](a T[2,2](b T[1,3](c . .) .) .) if this = Node('a, Node('b, Node('c), End), End)" in {
      Node('a, Node('b, Node('c), End), End).layoutBinaryTree must beEqualTo(PositionedNode('a, PositionedNode('b, PositionedNode('c, End, End, 1, 3), End, 2, 2), End, 3, 1))
    }

    "returns T[1,1](a . T[2,2](b . T[3,3](c . .))) if this = Node('a, End, Node('b, End, Node('c)))" in {
      Node('a, End, Node('b, End, Node('c))).layoutBinaryTree must beEqualTo(PositionedNode('a, End, PositionedNode('b, End, PositionedNode('c, End, End, 3, 3), 2, 2), 1, 1))
    }

    "returns T[3,1]('a' T[1,2]('b' . T[2,3]('c' . .)) T[4,2]('d' . .)) if this = Node('a', Node('b', End, Node('c')), Node('d'))" in {
      Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree must beEqualTo(PositionedNode('a', PositionedNode('b', End, PositionedNode('c', End, End, 2, 3), 1, 2), PositionedNode('d', End, End, 4, 2), 3, 1))
    }

    "returns T[8,1]('n' T[6,2]('k' . ...) T[12,2]('u' . ...)) if this = Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q'))" in {
      val expected =
        PositionedNode('n',
          PositionedNode('k',
            PositionedNode('c',
              PositionedNode('a',
                End,
                End,
                1, 4
                ),
              PositionedNode('h',
                PositionedNode('g',
                  PositionedNode('e',
                    End,
                    End,
                    3, 6
                    ),
                  End,
                  4, 5
                  ),
                End,
                5, 4
                ),
              2, 3
              ),
            PositionedNode('m',
              End,
              End,
              7, 3
              ),
            6, 2
            ),
          PositionedNode('u',
            PositionedNode('p',
              End,
              PositionedNode('s',
                PositionedNode('q',
                  End,
                  End,
                  10, 5
                  ),
                End,
                11, 4
                ),
              9, 3
              ),
            End,
            12, 2
            ),
          8, 1
        )

      Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree must beEqualTo(expected)
    }
  }

  "Tree#layoutBinaryTree2" should {
    "returns End if this = End" in {
      End.layoutBinaryTree2 must beEqualTo(End)
    }

    "returns T[1,1](a . .) if this = Node('a)" in {
      Node('a).layoutBinaryTree2 must beEqualTo(PositionedNode('a, End, End, 1, 1))
    }

    "returns T[2,1](a T[1,2](b . .) .) if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).layoutBinaryTree2 must beEqualTo(PositionedNode('a, PositionedNode('b, End, End, 1, 2), End, 2, 1))
    }

    "returns T[1,1](a . T[2,2](c . .)) if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).layoutBinaryTree2 must beEqualTo(PositionedNode('a, End, PositionedNode('c, End, End, 2, 2), 1, 1))
    }

    "returns T[2,1](a T[1,2](b . .) T[3,2](c . .)) if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).layoutBinaryTree2 must beEqualTo(PositionedNode('a, PositionedNode('b, End, End, 1, 2), PositionedNode('c, End, End, 3, 2), 2, 1))
    }

    "returns T[4,1](a T[2,2](b T[1,3](c . .) .) .) if this = Node('a, Node('b, Node('c), End), End)" in {
      Node('a, Node('b, Node('c), End), End).layoutBinaryTree2 must beEqualTo(PositionedNode('a, PositionedNode('b, PositionedNode('c, End, End, 1, 3), End, 2, 2), End, 4, 1))
    }

    "returns T[1,1](a . T[3,2](b . T[4,3](c . .))) if this = Node('a, End, Node('b, End, Node('c)))" in {
      Node('a, End, Node('b, End, Node('c))).layoutBinaryTree2 must beEqualTo(PositionedNode('a, End, PositionedNode('b, End, PositionedNode('c, End, End, 4, 3), 3, 2), 1, 1))
    }

    "returns T[3,1]('a T[1,2]('b' . T[2,3]('c' . .)) T[5,2]('d' . .)) if this = Node('a', Node('b', End, Node('c')), Node('d'))" in {
      Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2 must beEqualTo(PositionedNode('a', PositionedNode('b', End, PositionedNode('c', End, End, 2, 3), 1, 2), PositionedNode('d', End, End, 5, 2), 3, 1))
    }

    "returns T[15,1]('n' T[7,2]('k' . ...) T[23,2]('u' . ... ))if this = Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q'))" in {
      val expected =
        PositionedNode('n',
          PositionedNode('k',
            PositionedNode('c',
              PositionedNode('a',
                End,
                End,
                1, 4
                ),
              PositionedNode('e',
                PositionedNode('d',
                  End,
                  End,
                  4, 5
                  ),
                PositionedNode('g',
                  End,
                  End,
                  6, 5
                  ),
                5, 4
                ),
              3, 3
              ),
            PositionedNode('m',
              End,
              End,
              11, 3
              ),
            7, 2
            ),
          PositionedNode('u',
            PositionedNode('p',
              End,
              PositionedNode('q',
                End,
                End,
                21, 4
                ),
              19, 3
              ),
            End,
            23, 2
            ),
          15, 1
        )

      Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')).layoutBinaryTree2 must beEqualTo(expected)
    }
  }

  "Tree#toString2" should {
    """returns "" if this = End""" in {
      End.toString2 must beEqualTo("")
    }

    """returns "a" if this = Node('a')""" in {
      Node('a').toString2 must beEqualTo("a")
    }

    """returns "a(b,)" if this = Node('a', Node('b'), End)""" in {
      Node('a', Node('b'), End).toString2 must beEqualTo("a(b,)")
    }

    """returns "a(,c)" if this = Node('a', End, Node('c'))""" in {
      Node('a', End, Node('c')).toString2 must beEqualTo("a(,c)")
    }

    """returns "a(b,c)" if this = Node('a', Node('b'), Node('c'))""" in {
      Node('a', Node('b'), Node('c')).toString2 must beEqualTo("a(b,c)")
    }

    """returns "a(b(c,),)" if this = Node('a', Node('b', Node('c'), End), End)""" in {
      Node('a', Node('b', Node('c'), End), End).toString2 must beEqualTo("a(b(c,),)")
    }

    """returns "a(,b(,c))" if this = Node('a', End, Node('b', End, Node('c')))""" in {
      Node('a', End, Node('b', End, Node('c'))).toString2 must beEqualTo("a(,b(,c))")
    }

    """returns "a(b(,c),d)" if this = Node('a', Node('b', End, Node('c')), Node('d'))""" in {
      Node('a', Node('b', End, Node('c')), Node('d')).toString2 must beEqualTo("a(b(,c),d)")
    }

    """returns a(b(d,e),c(,f(g,))) if this = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))""" in {
      Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString2 must beEqualTo("a(b(d,e),c(,f(g,)))")
    }
  }

  "Tree->fromString" should {
    """returns End if s = """"" in {
      Tree.fromString("") must beEqualTo(End)
    }

    """returns Node('a') if s = "a"""" in {
      Tree.fromString("a") must beEqualTo(Node('a'))
    }

    """returns Node('a', Node('b'), End) if s = "a(b,)"""" in {
      Tree.fromString("a(b,)") must beEqualTo(Node('a', Node('b'), End))
    }

    """returns Node('a', End, Node('c')) if s = "a(,c)"""" in {
      Tree.fromString("a(,c)") must beEqualTo(Node('a', End, Node('c')))
    }

    """returns Node('a', Node('b'), Node('c')) if s = "a(b,c)"""" in {
      Tree.fromString("a(b,c)") must beEqualTo(Node('a', Node('b'), Node('c')))
    }

    """returns Node('a', Node('b', Node('c'), End), End) if s = "a(b(c,),)"""" in {
      Tree.fromString("a(b(c,),)") must beEqualTo(Node('a', Node('b', Node('c'), End), End))
    }

    """returns Node('a', End, Node('b', End, Node('c'))) if s = "a(,b(,c))"""" in {
      Tree.fromString("a(,b(,c))") must beEqualTo(Node('a', End, Node('b', End, Node('c'))))
    }

    """returns Node('a', Node('b', End, Node('c')), Node('d')) if s = "a(b(,c),d)"""" in {
      Tree.fromString("a(b(,c),d)") must beEqualTo(Node('a', Node('b', End, Node('c')), Node('d')))
    }

    """returns Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))) if s = "a(b(d,e),c(,f(g,)))"""" in {
      Tree.fromString("a(b(d,e),c(,f(g,)))") must beEqualTo(Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
    }
  }

  "Tree#preorder" should {
    "returns [] if this = End" in {
      End.preorder must beEmpty
    }

    "returns [a] if this = Node('a)" in {
      Node('a).preorder must beEqualTo(List('a))
    }

    "returns [a, b] if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).preorder must beEqualTo(List('a, 'b))
    }

    "returns [a, c] if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).preorder must beEqualTo(List('a, 'c))
    }

    "returns [a, b, c] if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).preorder must beEqualTo(List('a, 'b, 'c))
    }

    "returns [a, b, c] if this = Node('a, Node('b, Node('c), End), End)" in {
      Node('a, Node('b, Node('c), End), End).preorder must beEqualTo(List('a, 'b, 'c))
    }

    "returns [a, b, c] if this = Node('a', End, Node('b, End, Node('c)))" in {
      Node('a, End, Node('b, End, Node('c))).preorder must beEqualTo(List('a, 'b, 'c))
    }

    "returns [a, b, c, d] if this = Node('a, Node('b, End, Node('c)), Node('d))" in {
      Node('a, Node('b, End, Node('c)), Node('d)).preorder must beEqualTo(List('a, 'b, 'c, 'd))
    }

    "returns [a, b, d, e, c, f, g] if this = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))" in {
      Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).preorder must beEqualTo(List('a', 'b', 'd', 'e', 'c', 'f', 'g'))
    }
  }

  "Tree#inorder" should {
    "returns [] if this = End" in {
      End.inorder must beEmpty
    }

    "returns [a] if this = Node('a)" in {
      Node('a).inorder must beEqualTo(List('a))
    }

    "returns [b, a] if this = Node('a, Node('b), End)" in {
      Node('a, Node('b), End).inorder must beEqualTo(List('b, 'a))
    }

    "returns [a, c] if this = Node('a, End, Node('c))" in {
      Node('a, End, Node('c)).inorder must beEqualTo(List('a, 'c))
    }

    "returns [b, a, c] if this = Node('a, Node('b), Node('c))" in {
      Node('a, Node('b), Node('c)).inorder must beEqualTo(List('b, 'a, 'c))
    }

    "returns [c, b, a] if this = Node('a, Node('b, Node('c), End), End)" in {
      Node('a, Node('b, Node('c), End), End).inorder must beEqualTo(List('c, 'b, 'a))
    }

    "returns [a, b, c] if this = Node('a', End, Node('b, End, Node('c)))" in {
      Node('a, End, Node('b, End, Node('c))).inorder must beEqualTo(List('a, 'b, 'c))
    }

    "returns [b, c, a, d] if this = Node('a, Node('b, End, Node('c)), Node('d))" in {
      Node('a, Node('b, End, Node('c)), Node('d)).inorder must beEqualTo(List('b, 'c, 'a, 'd))
    }

    "returns [d, b, e, a, c, g, f] if this = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))" in {
      Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).inorder must beEqualTo(List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
    }
  }

  "Tree->preInTree(List[A], List[A])" should {
    "returns End if (preList, inList) = ([], [])" in {
      Tree.preInTree(Nil, Nil) must beEqualTo(End)
    }

    "returns End if (preList, inList) = ([], [a])" in {
      Tree.preInTree(Nil, List('a)) must beEqualTo(End)
    }

    "returns End if (preList, inList) = ([a], [])" in {
      Tree.preInTree(List('a), Nil) must beEqualTo(End)
    }

    "returns End if (preList, inList) = ([a, b], [a])" in {
      Tree.preInTree(List('a, 'b), List('a)) must beEqualTo(End)
    }

    "returns End if (preList, inList) = ([a], [a, b])" in {
      Tree.preInTree(List('a), List('a, 'b)) must beEqualTo(End)
    }

    "returns Node('a) if (preList, inList) = ([a], [a])" in {
      Tree.preInTree(List('a), List('a)) must beEqualTo(Node('a))
    }

    "returns Node('a, Node('b), End) if (preList, inList) = ([a, b], [b, a])" in {
      Tree.preInTree(List('a, 'b), List('b, 'a)) must beEqualTo(Node('a, Node('b), End))
    }

    "returns Node('a, End, Node('c)) if (preList, inList) = ([a, c], [a, c])" in {
      Tree.preInTree(List('a, 'c), List('a, 'c)) must beEqualTo(Node('a, End, Node('c)))
    }

    "returns Node('a, Node('b), Node('c)) if (preList, inList) = ([a, b, c], [b, a, c])" in {
      Tree.preInTree(List('a, 'b, 'c), List('b, 'a, 'c)) must beEqualTo(Node('a, Node('b), Node('c)))
    }

    "returns Node('a, Node('b, Node('c), End), End) if (preList, inList) = ([a, b, c], [c, b, a])" in {
      Tree.preInTree(List('a, 'b, 'c), List('c, 'b, 'a)) must beEqualTo(Node('a, Node('b, Node('c), End), End))
    }

    "returns Node('a, End, Node('b, End, Node('c))) if (preList, inList) = ([a, b, c], [a, b, c])" in {
      Tree.preInTree(List('a, 'b, 'c), List('a, 'b, 'c)) must beEqualTo(Node('a, End, Node('b, End, Node('c))))
    }

    "returns Node('a, Node('b, End, Node('c)), Node('d)) if (preList, inList) = ([a, b, c, d], [b, c, a, d])" in {
      Tree.preInTree(List('a, 'b, 'c, 'd), List('b, 'c, 'a, 'd)) must beEqualTo(Node('a, Node('b, End, Node('c)), Node('d)))
    }

    "returns Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))) if (preList, inList) = ([a, b, d, e, c, f, g], [d, b, e, a, c, g, f])" in {
      Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')) must beEqualTo(Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
    }
  }

  "Tree->preInTree(List[A], List[A]) check" should {
    "What happens if the same character appears in more than one node?" in {
      println(Tree.preInTree(List('a', 'b', 'a'), List('b', 'a', 'a')))
      true must beTrue
    }
  }
}
