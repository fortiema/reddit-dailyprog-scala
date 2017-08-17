object Main extends App {
  import scala.io.StdIn

  while (true) {
    val in = StdIn.readInt()

    if (testPrime(in)) {
      println(f"$in is prime!")
    } else {

      var primeBefore: Int = in - 1
      var primeAfter: Int = in + 1

      var beforeFound = false
      while (!beforeFound) {
        if (testPrime(primeBefore)) beforeFound = true else primeBefore -= 1
      }

      var afterFound = false
      while (!afterFound) {
        if (testPrime(primeAfter)) afterFound = true else primeAfter += 1
      }

      println(f"$primeBefore < $in < $primeAfter")
    }
  }

  def testPrime(n: Int): Boolean = {
    n match {
      case n if n <= 1 => false
      case n if n <= 3 => true
      case n if n % 2 == 0 || n % 3 == 0 => false
      case n =>
        var i = 5
        while ((i * i) <= n) {
          if (n % i == 0 || n % (i + 2) == 0) return false
          i += 6
        }
        true
    }
  }

}
