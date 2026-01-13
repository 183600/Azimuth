// 新增的测试用例，用于补充test_only包的测试覆盖率
test "add_with_zero" {
  // 测试与0相加的情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

test "add_with_min_value" {
  // 测试最小值的情况
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
}

test "add_positive_overflow" {
  // 测试正数溢出的情况
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
}

test "add_negative_overflow" {
  // 测试负数溢出的情况
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_with_zero_and_one" {
  // 测试与0和1相乘的情况
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
}

test "multiply_with_negative_one" {
  // 测试与-1相乘的情况
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(5, multiply(-5, -1))
  
  // 测试最小值与-1相乘的特殊情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
}

test "multiply_overflow" {
  // 测试乘法溢出的情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(46341, 46341))  // sqrt(max_val)的平方
  assert_eq(min_val, multiply(-46341, 46341))
}

test "greet_with_empty_string" {
  // 测试空字符串的情况
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  let result = greet("测试@123")
  assert_eq_string("Hello, 测试@123!", result)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}