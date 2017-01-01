// For computing Stirling's estimate of n! up to n=2000
import scala.math._
import scala.collection.mutable

def square(x: BigDecimal): BigDecimal =
  x * x

def binaryInt(n: Int): List[Char] = 
  n.toBinaryString.toList.reverse // n in binary

def power(r: BigDecimal, n: Int): BigDecimal = {
  val binaryn = binaryInt(n)
  val log2n = binaryn.length - 1
  val myMap = mutable.Map(0 -> BigDecimal(n % 2 + 1)) 
  // 1 or 2 depending on whether n is odd or even
  var rnew = r
  var rold = r
  var prod = myMap(0) 
  for (i <- (1 to log2n)) { // repeated squaring
    myMap(i) = square(rnew)
    rnew = square(rold)
    rold = rnew
    if (binaryn(i).toString == "1") {
      prod *= myMap(i)
    }
  }
  prod
}

def stirling(year: Int): BigDecimal = {
  val bigYear = BigDecimal(year)
  val denom = BigDecimal(exp(1))
  val sqrt = pow(2 * math.Pi * year, 0.5)
  power(bigYear / denom, year) * sqrt
}

def factorial(n: BigDecimal): BigDecimal = {
    def factorialAccumulator(acc: BigDecimal, n: BigDecimal): BigDecimal = {
        if (n == 0) acc
        else factorialAccumulator(n*acc, n-1)
    }
    factorialAccumulator(1, n)
}

println("100! = " + factorial(100))
println("100! ~ " + stirling(100))
println("300! = " + factorial(300))
println("300! ~ " + stirling(300))
println("1000! = " + factorial(1000))
println("1000! ~ " + stirling(1000))
println("1500! = " + factorial(1500))
println("1500! ~ " + stirling(1500))
println("2000! = " + factorial(2000))
println("2000! ~ " + stirling(2000))
println("2001! = " + factorial(2001))
println("2001! ~ " + stirling(2001) + ", dafuq?")
