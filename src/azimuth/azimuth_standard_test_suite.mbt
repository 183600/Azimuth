// Azimuth 标准测试套件
// 使用标准的 MoonBit 测试语法

// 测试基本加法功能
test "add_positive_numbers" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
  assert_eq(100, add(50, 50))
}

// 测试加法负数处理
test "add_negative_numbers" {
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-7, -3))
}

// 测试乘法功能
test "multiply_positive_numbers" {
  assert_eq(6, multiply(2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
}

// 测试乘法零值处理
test "multiply_with_zero" {
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
}

// 测试乘法负数处理
test "multiply_negative_numbers" {
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-15, multiply(3, -5))
  assert_eq(6, multiply(-2, -3))
}

// 测试问候功能
test "greet_with_names" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

// 测试空字符串问候
test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

// 测试向上取整除法 - 正数
test "divide_with_ceil_positive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(1, 2))
}

// 测试向上取整除法 - 边界情况
test "divide_with_ceil_edge_cases" {
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0
  assert_eq(0, divide_with_ceil(0, 5))  // 被除数为0
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}