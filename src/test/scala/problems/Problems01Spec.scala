package test.problems

import problems.WorkingWithLists
import problems.WorkingWithLists._
import org.specs2.mutable._
import java.util.NoSuchElementException

class WorkingWithListsSpec extends Specification {
  "last(List[A])" should {
    "throws NoSuchElementException if list = []" in {
      last[Int](Nil) must throwA[NoSuchElementException]
    }

    "returns 8 if list = [1, 1, 2, 3, 5, 8]" in {
      last(List(1, 1, 2, 3, 5, 8)) must beEqualTo(8)
    }
  }

  "penultimate(List[A])" should {
    "throws NoSuchElementException if list = []" in {
      penultimate[Int](Nil) must throwA[NoSuchElementException]
    }

    "throws NoSuchElementException if list = ['a]" in {
      penultimate(List('a)) must throwA[NoSuchElementException]
    }

    "returns 5 if list = [1, 1, 2, 3, 5, 8]" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must beEqualTo(5)
    }
  }

  "nth(Int, List[A])" should {
    "throws IndexOutOfBoundsException if (n, list) = (-1, [1, 1, 2, 3, 5, 8])" in {
      nth(-1, List(1, 1, 2, 3, 5, 8)) must throwA[IndexOutOfBoundsException]
    }

    "returns 1 if (n, list) = (0, [1, 1, 2, 3, 5, 8])" in {
      nth(0, List(1, 1, 2, 3, 5, 8)) must beEqualTo(1)
    }

    "returns 2 if (n, list) = (2, [1, 1, 2, 3, 5, 8])" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) must beEqualTo(2)
    }

    "throws IndexOutOfBoundsException if (n, list) = (0, [])" in {
      nth[String](0, Nil) must throwA[IndexOutOfBoundsException]
    }

    "throws IndexOutOfBoundsException if (n, list) = (6, [1, 1, 2, 3, 5, 8])" in {
      nth(6, List(1, 1, 2, 3, 5, 8)) must throwA[IndexOutOfBoundsException]
    }
  }

  "length(List[A])" should {
    "returns 0 if list = []" in {
      WorkingWithLists.length(Nil) must beEqualTo(0)
    }

    "returns 6 if list = [1, 1, 2, 3, 5, 8]" in {
      WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) must beEqualTo(6)
    }
  }

  "reverse(List[A])" should {
    "returns [] if list = []" in {
      reverse(Nil) must beEmpty
    }

    "returns [8, 5, 3, 2, 1, 1] if list = [1, 1, 2, 3, 5, 8]" in {
      reverse(List(1, 1, 2, 3, 5, 8)) must beEqualTo(List(8, 5, 3, 2, 1, 1))
    }
  }

  "isPalindrome(List[A])" should {
    "returns true if list = []" in {
      isPalindrome(Nil) must beTrue
    }

    "returns false if list = [1, 1, 2, 3, 5, 8]" in {
      isPalindrome(List(1, 1, 2, 3, 5, 8)) must beFalse
    }

    "returns true if list = [1, 2, 3, 2, 1]" in {
      isPalindrome(List(1, 2, 3, 2, 1)) must beTrue
    }
  }
}

