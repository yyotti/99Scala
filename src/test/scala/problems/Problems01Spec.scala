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

  "flatten(List[A])" should {
    "returns [] if list = []" in {
      flatten(Nil) must beEmpty
    }

    "returns [] if list = [[]]" in {
      flatten(List(List())) must beEmpty
    }

    "returns [] if list = [[], []]" in {
      flatten(List(List(), List())) must beEmpty
    }

    "returns [1, 2, 3] if list = [1, 2, 3]" in {
      flatten(List(1, 2, 3)) must beEqualTo(List(1, 2, 3))
    }

    "returns [1, 2, 3] if list = [1, [2, [3]]]" in {
      flatten(List(1, List(2, List(3)))) must beEqualTo(List(1, 2, 3))
    }

    "returns [1, 1, 2, 3, 5, 8] if list = [[1, 1], 2, [3, [5, 8]]" in {
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must beEqualTo(List(1, 1, 2, 3, 5, 8))
    }
  }

  "compress(List[A])" should {
    "returns [] if list = []" in {
      compress(Nil) must beEmpty
    }

    "returns [1] if list = [1]" in {
      compress(List(1)) must beEqualTo(List(1))
    }

    "returns [2] if list = [2, 2]" in {
      compress(List(2, 2)) must beEqualTo(List(2))
    }

    "returns [1, 2] if list = [1, 2]" in {
      compress(List(1, 2)) must beEqualTo(List(1, 2))
    }

    "returns [1, 2, 3] if list = [1, 1, 2, 3, 3]" in {
      compress(List(1, 1, 2, 3, 3)) must beEqualTo(List(1, 2, 3))
    }

    "returns ['a, 'b, 'c, 'a, 'd, 'e] if list = ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e]" in {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List('a, 'b, 'c, 'a, 'd, 'e))
    }
  }

  "pack(List[A])" should {
    "returns [] if list = []" in {
      pack(Nil) must beEmpty
    }

    "returns [[1]] if list = [1]" in {
      pack(List(1)) must beEqualTo(List(List(1)))
    }

    "returns [[2, 2]] if list = [2, 2]" in {
      pack(List(2, 2)) must beEqualTo(List(List(2, 2)))
    }

    "returns [[1], [2]] if list = [1, 2]" in {
      pack(List(1, 2)) must beEqualTo(List(List(1), List(2)))
    }

    "returns [[1, 1], [2], [3, 3]] if list = [1, 1, 2, 3, 3]" in {
      pack(List(1, 1, 2, 3, 3)) must beEqualTo(List(List(1, 1), List(2), List(3, 3)))
    }

    "returns [['a, 'a, 'a, 'a], ['b], ['c, 'c], ['a, 'a], ['d], ['e, 'e, 'e, 'e]] if list = ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e]" in {
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    }
  }

  "encode(List[A])" should {
    "returns [] if list = []" in {
      encode(Nil) must beEmpty
    }

    "returns [[1, 1]] if list = [1]" in {
      encode(List(1)) must beEqualTo(List((1, 1)))
    }

    "returns [[2, 2]] if list = [2, 2]" in {
      encode(List(2, 2)) must beEqualTo(List((2, 2)))
    }

    "returns [[1, 1], [1, 2]] if list = [1, 2]" in {
      encode(List(1, 2)) must beEqualTo(List((1, 1), (1, 2)))
    }

    "returns [[2, 1], [1, 2], [2, 3]] if list = [1, 1, 2, 3, 3]" in {
      encode(List(1, 1, 2, 3, 3)) must beEqualTo(List((2, 1), (1, 2), (2, 3)))
    }

    "returns [(4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)] if list = ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e]" in {
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    }
  }

  "encodeModified(List[A])" should {
    "returns [] if list = []" in {
      encodeModified(Nil) must beEmpty
    }

    "returns [1] if list = [1]" in {
      encodeModified(List(1)) must beEqualTo(List(1))
    }

    "returns [[2, 2]] if list = [2, 2]" in {
      encodeModified(List(2, 2)) must beEqualTo(List((2, 2)))
    }

    "returns [1, 2] if list = [1, 2]" in {
      encodeModified(List(1, 2)) must beEqualTo(List(1, 2))
    }

    "returns [[2, 1], 2, [2, 3]] if list = [1, 1, 2, 3, 3]" in {
      encodeModified(List(1, 1, 2, 3, 3)) must beEqualTo(List((2, 1), 2, (2, 3)))
    }

    "returns [(4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)] if list = ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e]" in {
      encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
    }
  }

  "decode(List[(Int, A)])" should {
    "returns [] if list = []" in {
      decode(Nil) must beEmpty
    }

    "returns [1] if list = [[1, 1]]" in {
      decode(List((1, 1))) must beEqualTo(List(1))
    }

    "returns [2, 2] if list = [[2, 2]]" in {
      decode(List((2, 2))) must beEqualTo(List(2, 2))
    }

    "returns [1, 2] if list = [[1, 1], [1, 2]]" in {
      decode(List((1, 1), (1, 2))) must beEqualTo(List(1, 2))
    }

    "returns [1, 1, 2, 3, 3] if list = [[2, 1], [1, 2], [2, 3]]" in {
      decode(List((2, 1), (1, 2), (2, 3))) must beEqualTo(List(1, 1, 2, 3, 3))
    }

    "returns ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e] if list = [(4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)]" in {
      decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) must beEqualTo(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }
  }

  "encodeDirect(List[A])" should {
    "returns [] if list = []" in {
      encodeDirect(Nil) must beEmpty
    }

    "returns [[1, 1]] if list = [1]" in {
      encodeDirect(List(1)) must beEqualTo(List((1, 1)))
    }

    "returns [[2, 2]] if list = [2, 2]" in {
      encodeDirect(List(2, 2)) must beEqualTo(List((2, 2)))
    }

    "returns [[1, 1], [1, 2]] if list = [1, 2]" in {
      encodeDirect(List(1, 2)) must beEqualTo(List((1, 1), (1, 2)))
    }

    "returns [[2, 1], [1, 2], [2, 3]] if list = [1, 1, 2, 3, 3]" in {
      encodeDirect(List(1, 1, 2, 3, 3)) must beEqualTo(List((2, 1), (1, 2), (2, 3)))
    }

    "returns [(4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)] if list = ['a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e]" in {
      encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must beEqualTo(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    }
  }

  "duplicate(List[A])" should {
    "returns [] if list = []" in {
      duplicate(Nil) must beEmpty
    }

    "returns [1, 1] if list = [1]" in {
      duplicate(List(1)) must beEqualTo(List(1, 1))
    }

    "returns [1, 1, 2, 2] if list = [1, 2]" in {
      duplicate(List(1, 2)) must beEqualTo(List(1, 1, 2, 2))
    }

    "returns ['a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd] if list = ['a, 'b, 'c, 'c, 'd]" in {
      duplicate(List('a, 'b, 'c, 'c, 'd)) must beEqualTo(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }
  }

  "duplicateN(Int, List[A])" should {
    "returns [] if (n, list) = (2, [])" in {
      duplicateN(2, Nil) must beEmpty
    }

    "returns [] if (n, list) = (0, [2])" in {
      duplicateN(0, List(2)) must beEmpty
    }

    "returns [1] if (n, list) = (1, [1])" in {
      duplicateN(1, List(1)) must beEqualTo(List(1))
    }

    "returns [1, 2] if (n, list) = (1, [1, 2])" in {
      duplicateN(1, List(1, 2)) must beEqualTo(List(1, 2))
    }

    "returns [1, 1] if (n, list) = (2, [1])" in {
      duplicateN(2, List(1)) must beEqualTo(List(1, 1))
    }

    "returns [1, 1, 2, 2] if (n, list) = (2, [1, 2])" in {
      duplicateN(2, List(1, 2)) must beEqualTo(List(1, 1, 2, 2))
    }

    "returns ['a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd] if (n, list) = (3, ['a, 'b, 'c, 'c, 'd])" in {
      duplicateN(3, List('a, 'b, 'c, 'c, 'd)) must beEqualTo(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    }

    "throws IllegalArgumentException if (n, list) = (-1, ['a, 'b, 'c, 'c, 'd])" in {
      duplicateN(-1, List('a, 'b, 'c, 'c, 'd)) must throwA[IllegalArgumentException]
    }
  }

  "drop(Int, List[A])" should {
    "returns [] if (n, list) = (2, [])" in {
      drop(2, Nil) must beEmpty
    }

    "returns [1, 2, 3] if (n, list) = (0, [1, 2, 3])" in {
      drop(0, List(1, 2, 3)) must beEqualTo(List(1, 2, 3))
    }

    "returns [] if (n, list) = (1, [1, 2])" in {
      drop(1, List(1, 2)) must beEmpty
    }

    "returns [1, 3, 5] if (n, list) = (2, [1, 2, 3, 4, 5])" in {
      drop(2, List(1, 2, 3, 4, 5)) must beEqualTo(List(1, 3, 5))
    }

    "returns ['a, 'b, 'd, 'e, 'g, 'h, 'j, 'k] if (n, list) = (3, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    }

    "returns ['a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd] if (n, list) = (3, ['a, 'b, 'c, 'c, 'd])" in {
      drop(3, List('a, 'b, 'c, 'c, 'd)) must beEqualTo(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    }
  }
}
