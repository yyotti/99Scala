package problems

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    // [テンプレート]
    //
    // /**
    //  * P31 (**) Determine whether a given integer number is prime.
    //  *
    //  * scala> 7.isPrime
    //  * res0: Boolean = true
    //  */
    // def isPrime: Boolean = ???

    /**
     * P31 (**) Determine whether a given integer number is prime.
     *
     * scala> 7.isPrime
     * res0: Boolean = true
     */
    def isPrime: Boolean = (start > 1) && primes.takeWhile { _ <= math.sqrt(start) }.forall { start % _ != 0 }

    /**
     * P33 (*) Determine whether two positive integer numbers are coprime.
     * Two numbers are coprime if their greatest common divisor equals 1.
     *
     * scala> 35.isCoprimeTo(64)
     * res0: Boolean = true
     */
    def isCoprimeTo(n: Int): Boolean = ???
  }

  object S99Int {
    import scala.language.implicitConversions
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter { _.isPrime }

    /**
     * P32 (**) Determine the greatest common divisor of two positive integer numbers.
     * Use Euclid's algorithm.
     *
     * scala> gcd(36, 63)
     * res0: Int = 9
     */
    def gcd(m: Int, n: Int): Int =
      if (m < 0 || n < 0) throw new IllegalArgumentException
      else if (m < n) gcd(n, m)
      else if (n == 0) m
      else gcd(n, m % n)
  }
}
