def pascal(m: Int, n: Int): Int = {
  if (m == 0 || n == 0 || m == n ) 1
  else pascal(m - 1, n - 1) + pascal(m - 1, n)
}
println(pascal(4, 2)) // 6
