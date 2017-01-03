// For computing Stirling's first order approximation of n! (error = O(1/n))
// It uses exponentiation by repeated squaring for efficiency
// https://en.wikipedia.org/wiki/Exponentiation_by_squaring
// Start writing your ScalaFiddle code here
import scala.math.{Pi, pow, exp}
import scala.collection.mutable

def square(x: BigDecimal): BigDecimal = x * x

def binaryInt(n: Int): List[Char] = 
  // n in binary
  n.toBinaryString.toList.reverse

def sqrt(x: Double): Double
  // returns the square root of x
  = pow(x, 0.5)

def power(r: BigDecimal, n: Int): BigDecimal = {
  val binaryn = binaryInt(n)
  val log2n = binaryn.length - 1
  var (rOld, rNew) = (r, r)
  var prod: BigDecimal = n % 2 match {
    case 0 => BigDecimal(1)
    case 1 => r
    case _ => BigDecimal(0)
  }
  var myMap = mutable.Map(0 -> prod) 
  
  // println(binaryn)
  for (i <- (1 to log2n)) {
    // Loop through repeated squaring 
    myMap(i) = square(rNew)
    rNew = square(rOld)
    rOld = rNew
    if (binaryn(i).toString == "1") {
      // println("index = " + i + ", product = " + prod)
      prod *= myMap(i)
    }
  }
  prod
}

 // Stirling's approximation for factorial: n! ~ sqrt(2 * pi * n) * (n / e) ^ n
def stirling(natural: Int): BigDecimal = {
  val bigDecNatural = BigDecimal(natural)
  val denom = BigDecimal(exp(1))
  val root = BigDecimal(sqrt(2 * Pi * natural))
  power(bigDecNatural / denom, natural) * root
}

// Tail-recursive function that computes n!
def factorial(n: BigDecimal): BigDecimal = {
    def factorialAccumulator(acc: BigDecimal, n: BigDecimal): BigDecimal = {
        if (n == 0) acc
        else factorialAccumulator(n*acc, n-1)
    }
    factorialAccumulator(1, n)
}

def stirDouble(number: Int): Double = {
  pow((number / exp(1)), number) * sqrt(2 * Pi * number)
}

def one_approx(number: Int): Unit = {
  val fact = factorial(number)
  val stir = stirling(number)
//   val stirDoub = stirDouble(number)
  val percentError = (fact - stir) / fact * 100
  println(number.toString() + "! = " + fact)
  println(number.toString() + "! ~ " + stir)
  // println(number.toString() + "! # " + stirDoub)
  println("Percent error = " + percentError + " %")
  println("-----------------------------------------")
}

def print_approximations(array: Array[Int]): Unit = {
  array.map{one_approx}
}

val ints = Array(10, 100, 123, 456, 789, 1000, 1234, 2345)
print_approximations(ints)

// 10! = 3628800
// 10! ~ 3598695.618741039025408117379456227
// Percent error = 0.8295960443937658342119328853552965 %
// -----------------------------------------
// 100! = 9.332621544394415268169923885626670E+157
// 100! ~ 9.324847625269423977235385604061917E+157
// Percent error = 0.08329834321483495860449290565984311 %
// -----------------------------------------
// 123! = 1.214630436702532967576624324188126E+205
// 123! ~ 1.213807796868444628197244339434850E+205
// Percent error = 0.06772758274703160677585307811781208 %
// -----------------------------------------
// 456! = 1.507773927777170659033285627982975E+1016
// 456! ~ 1.507498409516490239400840195294676E+1016
// Percent error = 0.01827318111851166334171194885150662 %
// -----------------------------------------
// 789! = 9.617974384062335467636723938025830E+1944
// 789! ~ 9.616958597627955685053569727290194E+1944
// Percent error = 0.01056133437060315553846891181116894 %
// -----------------------------------------
// 1000! = 4.023872600770937735437024339230024E+2567
// 1000! ~ 4.023537292037122933779125571234402E+2567
// Percent error = 0.008332985834356672366145256129141368 %
// -----------------------------------------
// 1234! = 5.108498146646957688130617626100440E+3280
// 1234! ~ 5.108153175986539166416304238706158E+3280
// Percent error = 0.006752878253365885615868758881465033 %
// -----------------------------------------
// 2017! = 4.691238606496835154123334763112338E+5791
// 2017! ~ 4.691044789704603372804763703612759E+5791
// Percent error = 0.004131463105785473608926075772276943 %
// -----------------------------------------
// Output generated by scalafiddle.io on 1/2/2017
