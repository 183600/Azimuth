// 综合测试用例 - 覆盖核心功能和边界情况

test "add_min_value_boundary" {
  // 测试最小值的边界情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值乘以2应该导致溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 最小值乘以-1应该导致溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最大值乘以2应该导致溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let x = 100
  let y = -50
  let z = 25
  
  // (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(1250, result1)
  
  // x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(-4975, result2)
}

test "string_greet_variations" {
  // 测试字符串拼接的各种情况
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "add_large_numbers" {
  // 测试大数相加
  let large1 = 1000000000
  let large2 = 1000000000
  let result = @azimuth.add(large1, large2)
  @azimuth.assert_eq(2000000000, result)
  
  // 测试接近边界的大数相加
  let near_max = 2147483640
  let small = 5
  let result2 = @azimuth.add(near_max, small)
  @azimuth.assert_eq(2147483645, result2)
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
}

test "add_sequence_operations" {
  // 测试连续加法操作
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(@azimuth.add(-10, 20), @azimuth.add(-5, 15))
  @azimuth.assert_eq(20, result2)
}

test "assert_functions_comprehensive" {
  // 综合测试断言函数
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 > 0)
  @azimuth.assert_false(1 < 0)
}