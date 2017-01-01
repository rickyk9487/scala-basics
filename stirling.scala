// For computing Stirling's estimate of n! up to n=2000
import scala.math.{Pi, pow, exp}
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
  val sqrt = pow(2 * Pi * year, 0.5)
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


// 100! = 9.332621544394415268169923885626670E+157
// 100! ~ 9.324847625269423977235385604061917E+157
// 300! = 3.060575122164406360353704612972684E+614
// 300! ~ 3.059725080789925813530177596118429E+614
// 1000! = 4.023872600770937735437024339230024E+2567
// 1000! ~ 4.023537292037122933779125571234402E+2567
// 1500! = 4.811997796779774860166990093581386E+4114
// 1500! ~ 4.811730470999212232931752975000442E+4114
// 2000! = 3.316275092450633241175393380576320E+5735
// 2000! ~ 3.316136917202177736152398512806653E+5735
// 2001! = 6.635866459993717115591962154533224E+5738
// 2001! ~ 1.802838982082949290885157482025880E+5736, dafuq?
