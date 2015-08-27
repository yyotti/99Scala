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

    "throws IllegalArgumentException if (n, list) = (-1, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      drop(-1, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must throwA[IllegalArgumentException]
    }
  }

  "split(Int, List[A])" should {
    "returns ([], []) if (n, list) = (2, [])" in {
      split(2, Nil) must beEqualTo((Nil, Nil))
    }

    "returns ([1, 2, 3], []) if (n, list) = (0, [1, 2, 3])" in {
      split(0, List(1, 2, 3)) must beEqualTo((Nil, List(1, 2, 3)))
    }

    "returns ([1], []) if (n, list) = (1, [1])" in {
      split(1, List(1)) must beEqualTo((List(1), Nil))
    }

    "returns ([1, 2], [3, 4, 5]) if (n, list) = (2, [1, 2, 3, 4, 5])" in {
      split(2, List(1, 2, 3, 4, 5)) must beEqualTo((List(1, 2), List(3, 4, 5)))
    }

    "returns (['a, 'b, 'c], ['d, 'e, 'f, 'g, 'h, 'i, 'j, 'k]) if (n, list) = (3, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    }

    "throws IndexOutOfBoundsException if (n, list) = (-1, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      split(-1, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must throwA[IndexOutOfBoundsException]
    }
  }

  "slice(Int, Int, List[A])" should {
    "returns [] if (i, k, list) = (1, 2, [])" in {
      slice(1, 2, Nil) must beEmpty
    }

    "returns [] if (i, k, list) = (2, 1, [1, 2, 3])" in {
      slice(2, 1, List(1, 2, 3)) must beEmpty
    }

    "returns [] if (i, k, list) = (2, 2, [1, 2, 3])" in {
      slice(2, 2, List(1, 2, 3)) must beEmpty
    }

    "returns [2] if (i, k, list) = (1, 2, [1, 2, 3])" in {
      slice(1, 2, List(1, 2, 3)) must beEqualTo(List(2))
    }

    "returns ['d, 'e, 'f, 'g] if (i, k, list) = (3, 7, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('d, 'e, 'f, 'g))
    }

    "returns [3, 4, 5] if (i, k, list) = (2, 6, [1, 2, 3, 4, 5])" in {
      slice(2, 6, List(1, 2, 3, 4, 5)) must beEqualTo(List(3, 4, 5))
    }

    "returns [] if (i, k, list) = (10, 12, [1, 2, 3, 4, 5])" in {
      slice(10, 12, List(1, 2, 3, 4, 5)) must beEmpty
    }

    "throws IndexOutOfBoundsException if (i, k, list) = (-1, 2, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      slice(-1, 2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must throwA[IndexOutOfBoundsException]
    }

    "throws IndexOutOfBoundsException if (i, k, list) = (2, -1, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      slice(2, -1, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must throwA[IndexOutOfBoundsException]
    }
  }

  "rotate(Int, List[A])" should {
    "returns [] if (n, list) = (2, [])" in {
      rotate(2, Nil) must beEmpty
    }

    "returns [] if (n, list) = (-2, [])" in {
      rotate(-2, Nil) must beEmpty
    }

    "returns [1, 2, 3] if (n, list) = (0, [1, 2, 3])" in {
      rotate(0, List(1, 2, 3)) must beEqualTo(List(1, 2, 3))
    }

    "returns [1] if (n, list) = (3, [1])" in {
      rotate(3, List(1)) must beEqualTo(List(1))
    }

    "returns [1] if (n, list) = (-3, [1])" in {
      rotate(-3, List(1)) must beEqualTo(List(1))
    }

    "returns ['d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c] if (n, list) = (3, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    }

    "returns ['j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i] if (n, list) = (-2, ['a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k])" in {
      rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) must beEqualTo(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    }

    "returns [3, 1, 2] if (n, list) = (5, [1, 2, 3])" in {
      rotate(5, List(1, 2, 3)) must beEqualTo(List(3, 1, 2))
    }

    "returns [3, 1, 2] if (n, list) = (-4, [1, 2, 3])" in {
      rotate(-4, List(1, 2, 3)) must beEqualTo(List(3, 1, 2))
    }
  }

  "removeAt(Int, List[A])" should {
    "throws IndexOutOfBoundsException if (n, list) = (-1, [1, 1, 2, 3, 5, 8])" in {
      removeAt(-1, List(1, 1, 2, 3, 5, 8)) must throwA[IndexOutOfBoundsException]
    }

    "returns [[], 1] if (n, list) = (0, [1])" in {
      removeAt(0, List(1)) must beEqualTo((Nil, 1))
    }

    "returns [[1, 2, 3, 5, 8], 1] if (n, list) = (0, [1, 1, 2, 3, 5, 8])" in {
      removeAt(0, List(1, 1, 2, 3, 5, 8)) must beEqualTo((List(1, 2, 3, 5, 8), 1))
    }

    "returns [['a, 'c, 'd], 'b] if (n, list) = (1, ['a, 'b, 'c, 'd])" in {
      removeAt(1, List('a, 'b, 'c, 'd)) must beEqualTo((List('a, 'c, 'd), 'b))
    }

    "throws IndexOutOfBoundsException if (n, list) = (0, [])" in {
      removeAt(0, Nil) must throwA[IndexOutOfBoundsException]
    }

    "throws IndexOutOfBoundsException if (n, list) = (6, [1, 1, 2, 3, 5, 8])" in {
      removeAt(6, List(1, 1, 2, 3, 5, 8)) must throwA[IndexOutOfBoundsException]
    }
  }

  "insertAt(A, Int, List[A])" should {
    "throws IndexOutOfBoundsException if (e, n, list) = (10, -1, [1, 1, 2, 3, 5, 8])" in {
      insertAt(10, -1, List(1, 1, 2, 3, 5, 8)) must throwA[IndexOutOfBoundsException]
    }

    "returns [1] if (e, n, list) = (1, 0, [])" in {
      insertAt(1, 0, Nil) must beEqualTo(List(1))
    }

    "returns [10, 1, 2, 3] if (e, n, list) = (10, 0, [1, 2, 3])" in {
      insertAt(10, 0, List(1, 2, 3)) must beEqualTo(List(10, 1, 2, 3))
    }

    "returns ['a, 'new, 'b, 'c, 'd] if (e, n, list) = ('new, 1, ['a, 'b, 'c, 'd])" in {
      insertAt('new, 1, List('a, 'b, 'c, 'd)) must beEqualTo(List('a, 'new, 'b, 'c, 'd))
    }

    "returns [1] if (e, n, list) = (1, 2, [])" in {
      insertAt(1, 2, Nil) must beEqualTo(List(1))
    }

    "returns [1, 2, 3, 4] if (e, n, list) = (4, 10, [1, 2, 3])" in {
      insertAt(4, 10, List(1, 2, 3)) must beEqualTo(List(1, 2, 3, 4))
    }
  }

  "range(Int, Int)" should {
    "returns [] if (s, e) = (2, 1)" in {
      range(2, 1) must beEqualTo(Nil)
    }

    "returns [] if (s, e) = (-1, -2)" in {
      range(-1, -2) must beEqualTo(Nil)
    }

    "returns [3] if (s, e) = (3, 3)" in {
      range(3, 3) must beEqualTo(List(3))
    }

    "returns [-4] if (s, e) = (-4, -4)" in {
      range(-4, -4) must beEqualTo(List(-4))
    }

    "returns [4, 5, 6, 7, 8, 9] if (s, e) = (4, 9)" in {
      range(4, 9) must beEqualTo(List(4, 5, 6, 7, 8, 9))
    }

    "returns [-4, -3, -2] if (s, e) = (-4, -2)" in {
      range(-4, -2) must beEqualTo(List(-4, -3, -2))
    }

    "returns [-4, -3, -2, -1, 0, 1, 2, 3] if (s, e) = (-4, 3)" in {
      range(-4, 3) must beEqualTo(List(-4, -3, -2, -1, 0, 1, 2, 3))
    }

  }

  "randomSelect(Int, List[A])" should {
    "throws IllegalArgumentException if (n, list) = (-1, [1, 2, 3])" in {
      randomSelect(-1, List(1, 2, 3)) must throwA[IllegalArgumentException]
    }

    "throws IllegalArgumentException if (n, list) = (2, [1])" in {
      randomSelect(2, List(1)) must throwA[IllegalArgumentException]
    }

    "returns [] if (n, list) = (0, [1, 2, 3])" in {
      randomSelect(0, List(1, 2, 3)) must beEqualTo(Nil)
    }

    "returns [?, ?, ?] if (n, list) = (3, ['a, 'b, 'c, 'd, 'f, 'g, 'h])" in {
      val list1 = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
      val list2 = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))

      list1.size must beEqualTo(3)
      list2.size must beEqualTo(3)
      list1 must not(beEqualTo(list2))
    }
  }

  "lotto(Int, Int)" should {
    "throws IllegalArgumentException if (n, max) = (-1, 10)" in {
      lotto(-1, 10) must throwA[IllegalArgumentException]
    }

    "throws IllegalArgumentException if (n, max) = (3, 0)" in {
      lotto(3, -2) must throwA[IllegalArgumentException]
    }

    "throws IllegalArgumentException if (n, max) = (2, 1)" in {
      lotto(2, 1) must throwA[IllegalArgumentException]
    }

    "returns [] if (n, max) = (0, 10)" in {
      lotto(0, 10) must beEqualTo(Nil)
    }

    "returns [?, ?, ?, ?, ?, ?] if (n, max) = (6, 49)" in {
      val list1 = lotto(6, 49)
      val list2 = lotto(6, 49)

      list1.size must beEqualTo(6)
      list2.size must beEqualTo(6)
      list1 must not(beEqualTo(list2))
    }

  }

  "randomPermute(List[A])" should {
    "returns [] if list = []" in {
      randomPermute(Nil) must beEqualTo(Nil)
    }

    "returns [1] if list = [1]" in {
      randomPermute(List(1)) must beEqualTo(List(1))
    }

    "returns [?, ?, ?, ?, ?, ?] if list = ['a, 'b, 'c, 'd, 'e, 'f]" in {
      val list = List('a, 'b, 'c, 'd, 'e, 'f)

      val list1 = randomPermute(list)
      val list2 = randomPermute(list)

      list1.size must beEqualTo(6)
      list2.size must beEqualTo(6)
      list1.sortBy { _.toString } must beEqualTo(list)
      list2.sortBy { _.toString } must beEqualTo(list)
      list1 must not(beEqualTo(list2))
    }
  }
}
