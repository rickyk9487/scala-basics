/**
 * Prints the first 10 harmonic numbers using a Rational number class
 */
class Rational(n: Int, d: Int) extends AnyRef {
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
  private val g = gcd(n, d)

  val numer: Int = n / g
  val denom: Int = d / g
  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  def -(that: Rational) =
    new Rational(numer * that.denom - that.numer * denom,
      denom * that.denom)
  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  override def toString = "" + numer + "/" + denom
}

object harmonicNumbers {
  var x = new Rational(0, 1)
  var i = 1
  while (i <= 10) {
    x += new Rational(1, i)
    i += 1
    println(x + "\n")
  }
}

harmonicNumbers
