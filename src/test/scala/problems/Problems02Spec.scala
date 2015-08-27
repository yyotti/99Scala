package test.problems

import problems.Arithmetic
import problems.Arithmetic._
import problems.arithmetic._
import org.specs2.mutable._

class ArithmeticSpec extends Specification {
  "S99Int#isPrime" should {
    "returns true if 2" in {
      new S99Int(2).isPrime must beTrue
    }

    "returns true if 3" in {
      new S99Int(3).isPrime must beTrue
    }

    "returns false if 4" in {
      new S99Int(4).isPrime must beFalse
    }

    "returns true if 5" in {
      new S99Int(5).isPrime must beTrue
    }

    "returns false if 6" in {
      new S99Int(6).isPrime must beFalse
    }

    "returns true if 7" in {
      new S99Int(7).isPrime must beTrue
    }
  }
}
