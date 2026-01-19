// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let empty_string = ""
  let single_char = "A"
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let special_chars = "!@#$%^&*()"
  let mixed_content = "Test123!@#"
  
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, A!", greet(single_char))
  assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", greet(long_string))
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  assert_eq_string("Hello, Test123!@#!", greet(mixed_content))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}