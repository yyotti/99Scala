package test.problems

import problems.logic._
import org.specs2.mutable._

class LogicAndCodesSpec extends Specification {
  "S99Logic->and(Boolean, Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      S99Logic.and(true, true) must beTrue
    }

    "returns false if (a, b) = (true, false)" in {
      S99Logic.and(true, false) must beFalse
    }

    "returns false if (a, b) = (false, true)" in {
      S99Logic.and(false, true) must beFalse
    }

    "returns false if (a, b) = (false, false)" in {
      S99Logic.and(false, false) must beFalse
    }
  }

  "S99Logic->or(Boolean, Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      S99Logic.or(true, true) must beTrue
    }

    "returns true if (a, b) = (true, false)" in {
      S99Logic.or(true, false) must beTrue
    }

    "returns true if (a, b) = (false, true)" in {
      S99Logic.or(false, true) must beTrue
    }

    "returns false if (a, b) = (false, false)" in {
      S99Logic.or(false, false) must beFalse
    }
  }

  "S99Logic->nand(Boolean, Boolean)" should {
    "returns false if (a, b) = (true, true)" in {
      S99Logic.nand(true, true) must beFalse
    }

    "returns true if (a, b) = (true, false)" in {
      S99Logic.nand(true, false) must beTrue
    }

    "returns true if (a, b) = (false, true)" in {
      S99Logic.nand(false, true) must beTrue
    }

    "returns true if (a, b) = (false, false)" in {
      S99Logic.nand(false, false) must beTrue
    }
  }

  "S99Logic->nor(Boolean, Boolean)" should {
    "returns false if (a, b) = (true, true)" in {
      S99Logic.nor(true, true) must beFalse
    }

    "returns false if (a, b) = (true, false)" in {
      S99Logic.nor(true, false) must beFalse
    }

    "returns false if (a, b) = (false, true)" in {
      S99Logic.nor(false, true) must beFalse
    }

    "returns true if (a, b) = (false, false)" in {
      S99Logic.nor(false, false) must beTrue
    }
  }

  "S99Logic->xor(Boolean, Boolean)" should {
    "returns false if (a, b) = (true, true)" in {
      S99Logic.xor(true, true) must beFalse
    }

    "returns true if (a, b) = (true, false)" in {
      S99Logic.xor(true, false) must beTrue
    }

    "returns true if (a, b) = (false, true)" in {
      S99Logic.xor(false, true) must beTrue
    }

    "returns false if (a, b) = (false, false)" in {
      S99Logic.xor(false, false) must beFalse
    }
  }

  "S99Logic->impl(Boolean, Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      S99Logic.impl(true, true) must beTrue
    }

    "returns false if (a, b) = (true, false)" in {
      S99Logic.impl(true, false) must beFalse
    }

    "returns true if (a, b) = (false, true)" in {
      S99Logic.impl(false, true) must beTrue
    }

    "returns true if (a, b) = (false, false)" in {
      S99Logic.impl(false, false) must beTrue
    }
  }

  "S99Logic->equ(Boolean, Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      S99Logic.equ(true, true) must beTrue
    }

    "returns false if (a, b) = (true, false)" in {
      S99Logic.equ(true, false) must beFalse
    }

    "returns true if (a, b) = (false, true)" in {
      S99Logic.equ(false, true) must beFalse
    }

    "returns true if (a, b) = (false, false)" in {
      S99Logic.equ(false, false) must beTrue
    }
  }

  "S99Logic->table2((Boolean, Boolean) => Boolean)" should {
    "prints table [and(a, or(a, b))]" in {
      implicit val out = new java.io.ByteArrayOutputStream()
      val expected = """
A     B     result
true  true  true
true  false true
false true  false
false false false
      """.trim

      S99Logic.table2((a: Boolean, b: Boolean) => a && (a || b))
      out.toString.trim must beEqualTo(expected)
    }
  }
}
