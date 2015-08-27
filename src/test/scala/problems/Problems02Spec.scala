package test.problems

import problems.Arithmetic
import problems.Arithmetic._
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

  "gcd(Int, Int)" should {
    "throws IllegalArgumentException if (m, n) = (-1, 2)" in {
      gcd(-1, 2) must throwA[IllegalArgumentException]
    }

    "returns 2 if (m, n) = (0, 2)" in {
      gcd(0, 2) must beEqualTo(2)
    }

    "throws IllegalArgumentException if (m, n) = (1, -2)" in {
      gcd(1, -2) must throwA[IllegalArgumentException]
    }

    "returns 1 if (m, n) = (1, 0)" in {
      gcd(1, 0) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (1, 1)" in {
      gcd(1, 1) must beEqualTo(1)
    }

    "returns 2 if (m, n) = (2, 2)" in {
      gcd(2, 2) must beEqualTo(2)
    }

    "returns 1 if (m, n) = (2, 1)" in {
      gcd(2, 1) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (1, 2)" in {
      gcd(1, 2) must beEqualTo(1)
    }

    "returns 1 if (m, n) = (3, 2)" in {
      gcd(3, 2) must beEqualTo(1)
    }

    "returns 2 if (m, n) = (6, 4)" in {
      gcd(6, 2) must beEqualTo(2)
    }

    "returns 9 if (m, n) = (36, 63)" in {
      gcd(36, 63) must beEqualTo(9)
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
      new S99Int(2).isCoprimeTo(4) must beTrue
    }

    "returns false if (this, n) = (4, 2)" in {
      new S99Int(4).isCoprimeTo(2) must beTrue
    }

    "returns false if (this, n) = (4, 6)" in {
      new S99Int(4).isCoprimeTo(6) must beTrue
    }

    "returns false if (this, n) = (6, 4)" in {
      new S99Int(6).isCoprimeTo(4) must beTrue
    }

    "returns true if (this, n) = (35, 64)" in {
      new S99Int(35).isCoprimeTo(64) must beTrue
    }
  }
}
