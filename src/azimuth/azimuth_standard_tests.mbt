// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 包含基础功能测试和边界情况测试

test "add_positive_numbers" {
  // 测试正数加法
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(3, -5))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(6, multiply(-2, -3))
}

test "greet_standard_cases" {
  // 测试标准问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-3, divide_with_ceil(10, -3))
}

test "combined_operations" {
  // 测试组合运算
  let sum_result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, sum_result)
  
  let complex_result = add(divide_with_ceil(15, 4), multiply(3, 2))
  assert_eq(10, complex_result)
}

test "edge_cases_zero" {
  // 测试零相关的边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "large_numbers" {
  // 测试大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
  assert_eq(334, divide_with_ceil(1000, 3))
}

test "unicode_greeting" {
  // 测试Unicode字符问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}