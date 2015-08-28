package test.problems

import problems.arithmetic._
import org.specs2.mutable._

class ArithmeticSpec extends Specification {
  "S99Int#isPrime" should {
    "returns false if this = -1" in {
      new S99Int(-1).isPrime must beFalse
    }

    "returns false if this = 0" in {
      new S99Int(0).isPrime must beFalse
    }

    "returns false if this = 1" in {
      new S99Int(1).isPrime must beFalse
    }

    "returns true if this = 2" in {
      new S99Int(2).isPrime must beTrue
    }

    "returns true if this = 3" in {
      new S99Int(3).isPrime must beTrue
    }

    "returns false if this = 4" in {
      new S99Int(4).isPrime must beFalse
    }

    "returns true if this = 5" in {
      new S99Int(5).isPrime must beTrue
    }

    "returns false if this = 6" in {
      new S99Int(6).isPrime must beFalse
    }

    "returns true if this = 7" in {
      new S99Int(7).isPrime must beTrue
    }
  }

  "S99Int->primes" should {
    "first 10 terms are [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]" in {
      S99Int.primes.take(10).toList must beEqualTo(List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    }
  }

  "S99Int->gcd(Int, Int)" should {
    "throws IllegalArgumentException if (m, n) = (-1, 2)" in {
      S99Int.gcd(-1, 2) must throwA[IllegalArgumentException]
    }

    "returns 2 if (m, n) = (0, 2)" in {
      S99Int.gcd(0, 2) must beEqualTo(2)
    }

    "throws IllegalArgumentException if (m, n) = (1, -2)" in {
      S99Int.gcd(1, -2) must throwA[IllegalArgumentException]
    }

    "returns 1 if (m, n) = (1, 0)" in {
      S99Int.gcd(1, 0) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (1, 1)" in {
      S99Int.gcd(1, 1) must beEqualTo(1)
    }

    "returns 2 if (m, n) = (2, 2)" in {
      S99Int.gcd(2, 2) must beEqualTo(2)
    }

    "returns 1 if (m, n) = (2, 1)" in {
      S99Int.gcd(2, 1) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (1, 2)" in {
      S99Int.gcd(1, 2) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (3, 2)" in {
      S99Int.gcd(3, 2) must beEqualTo(1)
    }

    "returns 2 if (m, n) = (6, 4)" in {
      S99Int.gcd(6, 2) must beEqualTo(2)
    }

    "returns 9 if (m, n) = (36, 63)" in {
      S99Int.gcd(36, 63) must beEqualTo(9)
    }
  }

  "S99Int#isCoprimeTo" should {
    "returns false if (this, n) = (-1, 2)" in {
      new S99Int(-1).isCoprimeTo(2) must beFalse
    }

    "returns false if (this, n) = (0, 2)" in {
      new S99Int(0).isCoprimeTo(2) must beFalse
    }

    "returns false if (this, n) = (1, -2)" in {
      new S99Int(1).isCoprimeTo(-2) must beFalse
    }

    "returns true if (this, n) = (1, 0)" in {
      new S99Int(1).isCoprimeTo(0) must beTrue
    }

    "returns true if (this, n) = (2, 3)" in {
      new S99Int(2).isCoprimeTo(3) must beTrue
    }

    "returns true if (this, n) = (3, 2)" in {
      new S99Int(3).isCoprimeTo(2) must beTrue
    }

    "returns false if (this, n) = (2, 4)" in {
      new S99Int(2).isCoprimeTo(4) must beFalse
    }

    "returns false if (this, n) = (4, 2)" in {
      new S99Int(4).isCoprimeTo(2) must beFalse
    }

    "returns false if (this, n) = (4, 6)" in {
      new S99Int(4).isCoprimeTo(6) must beFalse
    }

    "returns false if (this, n) = (6, 4)" in {
      new S99Int(6).isCoprimeTo(4) must beFalse
    }

    "returns true if (this, n) = (35, 64)" in {
      new S99Int(35).isCoprimeTo(64) must beTrue
    }
  }

  "S99Int#totient" should {
    "returns 0 if this = -1" in {
      new S99Int(-1).totient must beEqualTo(0)
    }

    "returns 0 if this = 0" in {
      new S99Int(0).totient must beEqualTo(0)
    }

    "returns 0 if this = 1" in {
      new S99Int(1).totient must beEqualTo(0)
    }

    "returns 1 if this = 2" in {
      new S99Int(2).totient must beEqualTo(1)
    }

    "returns 2 if this = 3" in {
      new S99Int(3).totient must beEqualTo(2)
    }

    "returns 2 if this = 4" in {
      new S99Int(4).totient must beEqualTo(2)
    }

    "returns 4 if this = 5" in {
      new S99Int(5).totient must beEqualTo(4)
    }

    "returns 2 if this = 6" in {
      new S99Int(6).totient must beEqualTo(2)
    }

    "returns 6 if this = 7" in {
      new S99Int(7).totient must beEqualTo(6)
    }
  }

  "S99Int#primeFactors" should {
    "returns [] if this = -1" in {
      new S99Int(-1).primeFactors must beEmpty
    }

    "returns [] if this = 0" in {
      new S99Int(0).primeFactors must beEmpty
    }

    "returns [] if this = 1" in {
      new S99Int(1).primeFactors must beEmpty
    }

    "returns [2] if this = 2" in {
      new S99Int(2).primeFactors must beEqualTo(List(2))
    }

    "returns [3] if this = 3" in {
      new S99Int(3).primeFactors must beEqualTo(List(3))
    }

    "returns [2, 2] if this = 4" in {
      new S99Int(4).primeFactors must beEqualTo(List(2, 2))
    }

    "returns [5] if this = 5" in {
      new S99Int(5).primeFactors must beEqualTo(List(5))
    }

    "returns [2, 3] if this = 6" in {
      new S99Int(6).primeFactors must beEqualTo(List(2, 3))
    }

    "returns [7] if this = 7" in {
      new S99Int(7).primeFactors must beEqualTo(List(7))
    }

    "returns [2, 2, 2] if this = 8" in {
      new S99Int(8).primeFactors must beEqualTo(List(2, 2, 2))
    }

    "returns [3, 3, 5, 7] if this = 315" in {
      new S99Int(315).primeFactors must beEqualTo(List(3, 3, 5, 7))
    }
  }

  "S99Int#primeFactorMultiplicity" should {
    "returns [] if this = -1" in {
      new S99Int(-1).primeFactorMultiplicity must beEmpty
    }

    "returns [] if this = 0" in {
      new S99Int(0).primeFactorMultiplicity must beEmpty
    }

    "returns [] if this = 1" in {
      new S99Int(1).primeFactorMultiplicity must beEmpty
    }

    "returns [2 -> 1] if this = 2" in {
      new S99Int(2).primeFactorMultiplicity must beEqualTo(Map(2 -> 1))
    }

    "returns [3 -> 1] if this = 3" in {
      new S99Int(3).primeFactorMultiplicity must beEqualTo(Map(3 -> 1))
    }

    "returns [2 -> 2] if this = 4" in {
      new S99Int(4).primeFactorMultiplicity must beEqualTo(Map(2 -> 2))
    }

    "returns [5 -> 1] if this = 5" in {
      new S99Int(5).primeFactorMultiplicity must beEqualTo(Map(5 -> 1))
    }

    "returns [2 -> 1, 3 -> 1] if this = 6" in {
      new S99Int(6).primeFactorMultiplicity must beEqualTo(Map(2 -> 1, 3 -> 1))
    }

    "returns [7 -> 1] if this = 7" in {
      new S99Int(7).primeFactorMultiplicity must beEqualTo(Map(7 -> 1))
    }

    "returns [2 -> 3] if this = 8" in {
      new S99Int(8).primeFactorMultiplicity must beEqualTo(Map(2 -> 3))
    }

    "returns [3 -> 2, 5 -> 1, 7 -> 1] if this = 315" in {
      new S99Int(315).primeFactorMultiplicity must beEqualTo(Map(3 -> 2, 5 -> 1, 7 -> 1))
    }
  }

  "S99Int#totientImproved" should {
    "returns 0 if this = -1" in {
      new S99Int(-1).totientImproved must beEqualTo(0)
    }

    "returns 0 if this = 0" in {
      new S99Int(0).totientImproved must beEqualTo(0)
    }

    "returns 0 if this = 1" in {
      new S99Int(1).totientImproved must beEqualTo(0)
    }

    "returns 1 if this = 2" in {
      new S99Int(2).totientImproved must beEqualTo(1)
    }

    "returns 2 if this = 3" in {
      new S99Int(3).totientImproved must beEqualTo(2)
    }

    "returns 2 if this = 4" in {
      new S99Int(4).totientImproved must beEqualTo(2)
    }

    "returns 4 if this = 5" in {
      new S99Int(5).totientImproved must beEqualTo(4)
    }

    "returns 2 if this = 6" in {
      new S99Int(6).totientImproved must beEqualTo(2)
    }

    "returns 6 if this = 7" in {
      new S99Int(7).totientImproved must beEqualTo(6)
    }
  }

  "S99Int->compareTotientFunctions" should {
    "test" in {
      // 特に何もしない
      S99Int.compareTotientFunctions(10090) must beEqualTo(())
    }
  }

  "S99Int->listPrimesinRange" should {
    "returns [] if r = (-1 to 1)" in {
      S99Int.listPrimesinRange(-1 to 1) must beEmpty
    }

    "returns [] if r = (1 to 1)" in {
      S99Int.listPrimesinRange(1 to 1) must beEmpty
    }

    "returns [2] if r = (1 to 2)" in {
      S99Int.listPrimesinRange(1 to 2) must beEqualTo(List(2))
    }

    "returns [2, 3, 5] if r = (1 to 6)" in {
      S99Int.listPrimesinRange(1 to 6) must beEqualTo(List(2, 3, 5))
    }

    "returns [7, 11, 13, 17, 19, 23, 29, 31] if r = (7 to 31)" in {
      S99Int.listPrimesinRange(7 to 31) must beEqualTo(List(7, 11, 13, 17, 19, 23, 29, 31))
    }
  }

  "S99Int#goldbach" should {
    "throws ArithmeticException if this = -1" in {
      new S99Int(-1).goldbach must throwA[ArithmeticException]
    }

    "throws ArithmeticException if this = 0" in {
      new S99Int(0).goldbach must throwA[ArithmeticException]
    }

    "throws ArithmeticException if this = 1" in {
      new S99Int(1).goldbach must throwA[ArithmeticException]
    }

    "throws ArithmeticException if this = 2" in {
      new S99Int(2).goldbach must throwA[ArithmeticException]
    }

    "throws ArithmeticException if this = 3" in {
      new S99Int(3).goldbach must throwA[ArithmeticException]
    }

    "returns (2, 2) if this = 4" in {
      new S99Int(4).goldbach must beEqualTo((2, 2))
    }

    "throws ArithmeticException if this = 5" in {
      new S99Int(5).goldbach must throwA[ArithmeticException]
    }

    "returns (3, 3) if this = 6" in {
      new S99Int(6).goldbach must beEqualTo((3, 3))
    }

    "returns (5, 23) if this = 28" in {
      new S99Int(28).goldbach must beEqualTo((5, 23))
    }
  }

  "S99Int->printGoldbachList(Range)" should {
    """prints "10 = 3 + 7\n12 = 5 + 7\n...20 = 3 + 17" if r = (9 to 20)""" in {
      val out = new java.io.ByteArrayOutputStream()
      val expected = """
      10 = 3 + 7
      12 = 5 + 7
      14 = 3 + 11
      16 = 3 + 13
      18 = 5 + 13
      20 = 3 + 17
      """.trim

      S99Int.printGoldbachList(9 to 20, out)
      out.toString must beEqualTo(expected)
    }
  }
}
