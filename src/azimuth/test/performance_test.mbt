test "performance_large_numbers" {
  let large_num = 46340  // sqrt(2^31-1) ≈ 46340，避免乘法溢出
  @azimuth.assert_eq(92680, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(2147395600, @azimuth.multiply(large_num, large_num))
}

///|
test "performance_repeated_operations" {
  let mut sum = 0
  sum = @azimuth.add(sum, 1)
  sum = @azimuth.add(sum, 2)
  sum = @azimuth.add(sum, 3)
  sum = @azimuth.add(sum, 4)
  sum = @azimuth.add(sum, 5)
  @azimuth.assert_eq(15, sum)
  
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
}

///|
test "performance_string_operations" {
  let long_name = "VeryLongNameThatTestsPerformance"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatTestsPerformance!", result)
}


