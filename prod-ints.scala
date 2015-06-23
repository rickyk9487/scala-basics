def product(f:Int => Int)(a: Int, b: Int): Int = {
  def prodF(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * prodF(a + 1, b)
  prodF(a, b)
  }
println(product(x => x)(3, 5)) // 60
}
