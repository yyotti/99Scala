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

  "S99Logic->not(Boolean)" should {
    "returns false if a = true" in {
      S99Logic.not(true) must beFalse
    }

    "returns true if a = false" in {
      S99Logic.not(false) must beTrue
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

  "S99Boolean#and(Boolean)" should {
    "returns true if (this, a) = (true, true)" in {
      new S99Boolean(true).and(true) must beTrue
    }

    "returns false if (this, a) = (true, false)" in {
      new S99Boolean(true).and(false) must beFalse
    }

    "returns false if (this, a) = (false, true)" in {
      new S99Boolean(false).and(true) must beFalse
    }

    "returns false if (this, a) = (false, false)" in {
      new S99Boolean(false).and(false) must beFalse
    }
  }

  "S99Boolean#or(Boolean)" should {
    "returns true if (this, a) = (true, true)" in {
      new S99Boolean(true).or(true) must beTrue
    }

    "returns true if (this, a) = (true, false)" in {
      new S99Boolean(true).or(false) must beTrue
    }

    "returns true if (this, a) = (false, true)" in {
      new S99Boolean(false).or(true) must beTrue
    }

    "returns false if (this, a) = (false, false)" in {
      new S99Boolean(false).or(false) must beFalse
    }
  }

  "S99Boolean#nand(Boolean)" should {
    "returns false if (this, a) = (true, true)" in {
      new S99Boolean(true).nand(true) must beFalse
    }

    "returns true if (this, a) = (true, false)" in {
      new S99Boolean(true).nand(false) must beTrue
    }

    "returns true if (this, a) = (false, true)" in {
      new S99Boolean(false).nand(true) must beTrue
    }

    "returns true if (this, a) = (false, false)" in {
      new S99Boolean(false).nand(false) must beTrue
    }
  }

  "S99Boolean#nor(Boolean)" should {
    "returns false if (a, b) = (true, true)" in {
      new S99Boolean(true).nor(true) must beFalse
    }

    "returns false if (a, b) = (true, false)" in {
      new S99Boolean(true).nor(false) must beFalse
    }

    "returns false if (a, b) = (false, true)" in {
      new S99Boolean(false).nor(true) must beFalse
    }

    "returns true if (a, b) = (false, false)" in {
      new S99Boolean(false).nor(false) must beTrue
    }
  }

  "S99Boolean#xor(Boolean)" should {
    "returns false if (a, b) = (true, true)" in {
      new S99Boolean(true).xor(true) must beFalse
    }

    "returns true if (a, b) = (true, false)" in {
      new S99Boolean(true).xor(false) must beTrue
    }

    "returns true if (a, b) = (false, true)" in {
      new S99Boolean(false).xor(true) must beTrue
    }

    "returns false if (a, b) = (false, false)" in {
      new S99Boolean(false).xor(false) must beFalse
    }
  }

  "S99Boolean#impl(Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      new S99Boolean(true).impl(true) must beTrue
    }

    "returns false if (a, b) = (true, false)" in {
      new S99Boolean(true).impl(false) must beFalse
    }

    "returns true if (a, b) = (false, true)" in {
      new S99Boolean(false).impl(true) must beTrue
    }

    "returns true if (a, b) = (false, false)" in {
      new S99Boolean(false).impl(false) must beTrue
    }
  }

  "S99Boolean#equ(Boolean)" should {
    "returns true if (a, b) = (true, true)" in {
      new S99Boolean(true).equ(true) must beTrue
    }

    "returns false if (a, b) = (true, false)" in {
      new S99Boolean(true).equ(false) must beFalse
    }

    "returns true if (a, b) = (false, true)" in {
      new S99Boolean(false).equ(true) must beFalse
    }

    "returns true if (a, b) = (false, false)" in {
      new S99Boolean(false).equ(false) must beTrue
    }
  }

  "S99Logic->table2((Boolean, Boolean) => Boolean)" should {
    "prints table [a and (a or b)]" in {
      import S99Logic._

      implicit val out = new java.io.ByteArrayOutputStream()
      val expected = """
A     B     result
true  true  true
true  false true
false true  false
false false false
      """.trim

      S99Logic.table2((a: Boolean, b: Boolean) => a and(a or b))
      out.toString.trim must beEqualTo(expected)
    }
  }

  "S99Logic->gray(Int)" should {
    "throws IllegalArgumentException if n = -1" in {
      S99Logic.gray(-1) must throwA[IllegalArgumentException]
    }

    """returns [""] if n = 0""" in {
      S99Logic.gray(0) must beEqualTo(List(""))
    }

    """returns ["0", "1"] if n = 1""" in {
      S99Logic.gray(1) must beEqualTo(List("0", "1"))
    }

    """returns ["00", "01", "11", "10"] if n = 2""" in {
      S99Logic.gray(2) must beEqualTo(List("00", "01", "11", "10"))
    }

    """returns ["000", "001", "011", "010", "110", "111", "101", "100"] if n = 3""" in {
      S99Logic.gray(3) must beEqualTo(List("000", "001", "011", "010", "110", "111", "101", "100"))
    }
  }

  "S99Logic->grayMemorized(Int)" should {
    "throws IllegalArgumentException if n = -1" in {
      S99Logic.grayMemorized(-1) must throwA[IllegalArgumentException]
    }

    """returns [""] if n = 0""" in {
      S99Logic.grayMemorized(0) must beEqualTo(List(""))
    }

    """returns ["0", "1"] if n = 1""" in {
      S99Logic.grayMemorized(1) must beEqualTo(List("0", "1"))
    }

    """returns ["00", "01", "11", "10"] if n = 2""" in {
      S99Logic.grayMemorized(2) must beEqualTo(List("00", "01", "11", "10"))
    }

    """returns ["000", "001", "011", "010", "110", "111", "101", "100"] if n = 3""" in {
      S99Logic.grayMemorized(3) must beEqualTo(List("000", "001", "011", "010", "110", "111", "101", "100"))
    }
  }

  "S99Logic->huffman(List[(A, Int)])" should {
    "returns [] if freqs = []" in {
      S99Logic.huffman(Nil) must beEmpty
    }

    """returns [("a", "0")] if freqs = [("a", 1)]""" in {
      S99Logic.huffman(List(("a", 1))) must beEqualTo(List(("a", "0")))
    }

    """returns [("a", "0")] if freqs = [("a", 3)]""" in {
      S99Logic.huffman(List(("a", 3))) must beEqualTo(List(("a", "0")))
    }

    """returns [("a", "0"), ("b", "1")] if freqs = [("a", 1), ("b", 1)]""" in {
      S99Logic.huffman(List(("a", 1), ("b", 1))) must beEqualTo(List(("a", "0"), ("b", "1")))
    }

    """returns [("b", "0"), ("a", "1")] if freqs = [("a", 1), ("b", 2)]""" in {
      S99Logic.huffman(List(("a", 1), ("b", 2))) must beEqualTo(List(("b", "0"), ("a", "1")))
    }

    """returns [("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")] if freqs = [("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)]""" in {
      S99Logic.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) must beEqualTo(List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")))
    }
  }
}
