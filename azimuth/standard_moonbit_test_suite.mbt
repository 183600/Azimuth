// 标准 MoonBit 测试套件
// 测试 azimuth 包的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(1, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  assert_eq(2, add(5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(-10, 10))
}

test "multiply_basic_cases" {
  // 测试基本乘法运算
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(8, multiply(-2, -4))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  assert_eq(10, multiply(1, 10))
  assert_eq(10, multiply(10, 1))
  assert_eq(-10, multiply(-1, 10))
  assert_eq(-10, multiply(1, -10))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 2))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "boolean_assertions" {
  // 测试布尔断言函数
  assert_true(1 > 0)
  assert_true(0 == 0)
  assert_false(1 < 0)
  assert_false(1 == 0)
}