package test.problems

import problems.multiwaytree._
import org.specs2.mutable._

class MultiwayTreesSpec extends Specification {
  "MTree#nodeCount" should {
    "returns 1 if this = MTree('a)" in {
      MTree('a).nodeCount must beEqualTo(1)
    }

    "returns 2 if this = MTree('a, List(MTree('b)))" in {
      MTree('a, List(MTree('b))).nodeCount must beEqualTo(2)
    }

    "returns 3 if this = MTree('a, List(MTree('b), MTree('c)))" in {
      MTree('a, List(MTree('b), MTree('c))).nodeCount must beEqualTo(3)
    }

    "returns 4 if this = MTree('a, List(MTree('b), MTree('c), MTree('d)))" in {
      MTree('a, List(MTree('b), MTree('c), MTree('d))).nodeCount must beEqualTo(4)
    }

    "returns 5 if this = MTree('a, List(MTree('b), MTree('c, List(MTree('e))), MTree('d)))" in {
      MTree('a, List(MTree('b), MTree('c, List(MTree('e))), MTree('d))).nodeCount must beEqualTo(5)
    }

    "returns 6 if this = MTree('a, List(MTree('b), MTree('c, List(MTree('e))), MTree('d, List(MTree('f)))))" in {
      MTree('a, List(MTree('b), MTree('c, List(MTree('e))), MTree('d, List(MTree('f))))).nodeCount must beEqualTo(6)
    }

    "returns 7 if this = MTree('a, List(MTree('b), MTree('c, List(MTree('e, MTree('g)))), MTree('d, List(MTree('f)))))" in {
      MTree('a, List(MTree('b), MTree('c, List(MTree('e), MTree('g))), MTree('d, List(MTree('f))))).nodeCount must beEqualTo(7)
    }

    """returns 7 if this = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))""" in {
      MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).nodeCount must beEqualTo(7)
    }
  }

  "MTree#toString2" should {
    """returns "a^" if this = MTree('a')""" in {
      MTree('a').toString2 must beEqualTo("a^")
    }

    """returns "ab^^" if this = MTree('a', List(MTree('b')))""" in {
      MTree('a', List(MTree('b'))).toString2 must beEqualTo("ab^^")
    }

    """returns "ab^c^^" if this = MTree('a', List(MTree('b'), MTree('c')))""" in {
      MTree('a', List(MTree('b'), MTree('c'))).toString2 must beEqualTo("ab^c^^")
    }

    """returns "ab^c^d^^" if this = MTree('a', List(MTree('b'), MTree('c'), MTree('d')))""" in {
      MTree('a', List(MTree('b'), MTree('c'), MTree('d'))).toString2 must beEqualTo("ab^c^d^^")
    }

    """returns "ab^ce^^d^^" if this = MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d')))""" in {
      MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d'))).toString2 must beEqualTo("ab^ce^^d^^")
    }

    """returns "ab^ce^^df^^^" if this = MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d', List(MTree('f')))))""" in {
      MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d', List(MTree('f'))))).toString2 must beEqualTo("ab^ce^^df^^^")
    }

    """returns "ab^ce^g^^df^^^" if this = MTree('a', List(MTree('b'), MTree('c', List(MTree('e'), MTree('g')))), MTree('d', List(MTree('f')))))""" in {
      MTree('a', List(MTree('b'), MTree('c', List(MTree('e'), MTree('g'))), MTree('d', List(MTree('f'))))).toString2 must beEqualTo("ab^ce^g^^df^^^")
    }

    """returns "afg^^c^bd^e^^^" if this = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))""" in {
      MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString2 must beEqualTo("afg^^c^bd^e^^^")
    }
  }

  "MTree->string2MTree" should {
    """throws IllegalArgumentException if this = "a"""" in {
      MTree.string2MTree("a") must throwA[IllegalArgumentException]
    }

    """returns MTree('a') if this = "a^"""" in {
      MTree.string2MTree("a^") must beEqualTo(MTree('a'))
    }

    """returns MTree('a', List(MTree('b'))) if this = "ab^^"""" in {
      MTree.string2MTree("ab^^") must beEqualTo(MTree('a', List(MTree('b'))))
    }

    """returns MTree('a', List(MTree('b'), MTree('c'))) if this = "ab^c^^"""" in {
      MTree.string2MTree("ab^c^^") must beEqualTo(MTree('a', List(MTree('b'), MTree('c'))))
    }

    """returns MTree('a', List(MTree('b'), MTree('c'), MTree('d'))) if this = "ab^c^d^^"""" in {
      MTree.string2MTree("ab^c^d^^") must beEqualTo(MTree('a', List(MTree('b'), MTree('c'), MTree('d'))))
    }

    """returns MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d'))) if this = "ab^ce^^d^^"""" in {
      MTree.string2MTree("ab^ce^^d^^") must beEqualTo(MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d'))))
    }

    """returns MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d', List(MTree('f'))))) if this = "ab^ce^^df^^^"""" in {
      MTree.string2MTree("ab^ce^^df^^^") must beEqualTo(MTree('a', List(MTree('b'), MTree('c', List(MTree('e'))), MTree('d', List(MTree('f'))))))
    }

    """returns MTree('a', List(MTree('b'), MTree('c', List(MTree('e', MTree('g')))), MTree('d', List(MTree('f'))))) if this = "ab^ce^g^^df^^^"""" in {
      MTree.string2MTree("ab^ce^g^^df^^^") must beEqualTo(MTree('a', List(MTree('b'), MTree('c', List(MTree('e'), MTree('g'))), MTree('d', List(MTree('f'))))))
    }

    """returns MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))) if this = "afg^^c^bd^e^^^"""" in {
      MTree.string2MTree("afg^^c^bd^e^^^") must beEqualTo(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))))
    }
  }

  "MTree#internalPathLength" should {
    import MTree._

    """returns 0 if this = "a^"""" in {
      "a^".internalPathLength must beEqualTo(0)
    }

    """returns 1 if this = "ab^^"""" in {
      "ab^^".internalPathLength must beEqualTo(1)
    }

    """returns 2 if this = "ab^c^^"""" in {
      "ab^c^^".internalPathLength must beEqualTo(2)
    }

    """returns 3 if this = "ab^c^d^^"""" in {
      "ab^c^d^^".internalPathLength must beEqualTo(3)
    }

    """returns 5 if this = "ab^ce^^d^^"""" in {
      "ab^ce^^d^^".internalPathLength must beEqualTo(5)
    }

    """returns 7 if this = "ab^ce^^df^^^"""" in {
      "ab^ce^^df^^^".internalPathLength must beEqualTo(7)
    }

    """returns 9 if this = "ab^ce^g^^df^^^"""" in {
      "ab^ce^g^^df^^^".internalPathLength must beEqualTo(9)
    }

    """returns 9 if this = "afg^^c^bd^e^^^"""" in {
      "afg^^c^bd^e^^^".internalPathLength must beEqualTo(9)
    }
  }
}
