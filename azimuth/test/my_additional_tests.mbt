// 额外的MoonBit测试用例
// 测试一些边界情况和特殊场景

test "add_with_minimum_values" {
  // 测试最小值相加的特殊情况
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 0)
  let result2 = @azimuth.add(0, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

test "add_mixed_signs" {
  // 测试正负数相加的各种情况
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "multiply_boundary_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
}

test "greet_with_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "assert_functions_with_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(2147483647, 2147483647)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "complex_addition_scenarios" {
  // 测试复杂的加法场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -100))
  
  // 零参与的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法的各种情况
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(-10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
}