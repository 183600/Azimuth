// Simple quality tests for azimuth library
// 简单的质量测试用例

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  assert_eq(0, multiply(0, 1000))
  assert_eq(1000, multiply(1, 1000))
  assert_eq(-1000, multiply(1, -1000))
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation" {
  // 测试复杂计算
  let result = add(multiply(2, 3), divide_with_ceil(10, 3))
  assert_eq(10, result)
}