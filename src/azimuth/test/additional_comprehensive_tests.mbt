test "add_boundary_values" {
  // 测试接近Int32边界的值
  let near_max = 2147483646
  let near_min = -2147483647
  
  @azimuth.assert_eq(2147483647, @azimuth.add(near_max, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(near_min, -1))
}

///|
test "add_overflow_handling" {
  // 测试溢出情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出应该返回最大值
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出应该返回最小值
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

///|
test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 1和-1的情况
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

///|
test "multiply_overflow_handling" {
  // 测试乘法溢出
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘应该返回边界值
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // 接近sqrt(max_val)
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

///|
test "min_value_special_cases" {
  // 测试最小值的特殊情况
  let min_val = -2147483648
  
  // 最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  
  // 最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

///|
test "greet_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

///|
test "complex_operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(900, result)  // (10 + 20) * 30 = 900
  
  // a * b + c
  let product_ab = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product_ab, c)
  @azimuth.assert_eq(230, result2)  // 10 * 20 + 30 = 230
}


