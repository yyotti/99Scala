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
  }
}
