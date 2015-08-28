package problems.arithmetic

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
  def isCoprimeTo(n: Int): Boolean = start >= 0 && n >= 0 && gcd(start, n) == 1

  /**
   * P34 (**) Calculate Euler's totient function phi(m).
   * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
   *
   * scala> 10.totient
   * res0: Int = 4
   */
  def totient: Int = if (start < 2) 0 else (1 until start).count { _.isCoprimeTo(start) }

  /**
   * P35 (**) Determine the prime factors of a given positive integer.
   * Construct a flat list containing the prime factors in ascending order.
   *
   * scala> 315.primeFactors
   * res0: List[Int] = List(3, 3, 5, 7)
   */
  def primeFactors: List[Int] =
    if (start < 2) Nil
    else {
      val p = primes.find { start % _ == 0 }.get
      p :: (start / p).primeFactors
    }

  /**
   * P36 (**) Determine the prime factors of a given positive integer (2).
   * Construct a list containing the prime factors and their multiplicity.
   *
   * scala> 315.primeFactorMultiplicity
   * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
   *
   * Alternately, use a Map for the result.
   *
   * scala> 315.primeFactorMultiplicity
   * res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
   */
  def primeFactorMultiplicity: Map[Int, Int] =
    primeFactors.foldLeft(Map[Int, Int]()) { case (map, p) => map + (p -> (map.getOrElse(p, 0) + 1)) }

  /**
   * P37 (**) Calculate Euler's totient function phi(m) (improved).
   * See problem P34 for the definition of Euler's totient function.
   * If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows:
   * Let [ [p1, m1], [p2, m2], [p3, m3], ... ] be the list of prime factors (and their multiplicities) of a given number m.
   * Then phi(m) can be calculated with the following formula:
   * phi(m) = (p1-1)*p1^(m1-1) * (p2-1)*p2^(m2-1) * (p3-1)*p3^(m3-1) * ...
   *
   * Note that a^b stands for the bth power of a.
   */
  def totientImproved: Int =
    if (start < 2) 0
    else primeFactorMultiplicity.map { case (p, m) => (p - 1) * BigInt(p).pow(m - 1).toInt }.product

  /**
   * P40 (**) Goldbach's conjecture.
   * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
   * E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
   * It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent).
   * Write a function to find the two prime numbers that sum up to a given even integer.
   *
   * scala> 28.goldbach
   * res0: (Int, Int) = (5,23)
   */
  def goldbach: (Int, Int) =
    if (start <= 2) throw new ArithmeticException
    else if (start % 2 == 1) throw new ArithmeticException
    else primes.takeWhile { _ <= start / 2 }.find { p => (start - p).isPrime }.map { p => (p, start - p) }.get
}

object S99Int {
  import java.io.OutputStream
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

  /**
   * P38 (*) Compare the two methods of calculating Euler's totient function.
   * Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example.
   */
  def compareTotientFunctions(n: Int): Unit = {
    def time[A](block: => A): (Long, A) = {
      val start = System.currentTimeMillis()
      val result = block
      val end = System.currentTimeMillis()

      (end - start, result)
    }

    // まず十分にprimesを生成しておく
    primes.takeWhile { _ <= n }

    val (time1, ret1) = time(n.totient)
    val (time2, ret2) = time(n.totientImproved)

    println(s"totient($n): $time1 [ms]")
    println(s"totient($n): $time2 [ms]")
  }

  /**
   * P39 (*) A list of prime numbers.
   * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
   *
   * scala> listPrimesinRange(7 to 31)
   * res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
   */
  def listPrimesinRange(r: Range): List[Int] = r.filter { _.isPrime }.toList

  /**
   * P41 (**) A list of Goldbach compositions.
   * Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
   *
   * scala> printGoldbachList(9 to 20)
   * 10 = 3 + 7
   * 12 = 5 + 7
   * 14 = 3 + 11
   * 16 = 3 + 13
   * 18 = 5 + 13
   * 20 = 3 + 17
   */
  def printGoldbachList(r: Range, out: OutputStream = Console.out): Unit =
    r.filter { n => n > 2 && n % 2 == 0 }.map { n => val (x, y) = n.goldbach; s"${n} = $x + $y" }.foreach { s => Console.withOut(out) { println(s) } }
}

