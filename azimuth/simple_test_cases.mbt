// 简单的 MoonBit 测试用例
test "simple_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_division" {
  assert_eq(3, divide_with_ceil(10, 4))
}

test "simple_zero_cases" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "azimuth_premium_quality_test" {
  // Azimuth 高级质量测试
  // 测试边界值和特殊情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
  assert_eq(1, divide_with_ceil(big_num, big_num))
  
  // 测试负数运算
  assert_eq(-2000000, add(-big_num, -big_num))
  assert_eq(1000000000000, multiply(-big_num, -big_num))
  assert_eq(-1, divide_with_ceil(-big_num, big_num))
}