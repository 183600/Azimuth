// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和库函数

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "add_function_with_zero" {
  // 测试与0相加的情况
  assert_eq(10, add(10, 0))
  assert_eq(10, add(0, 10))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_function_negative" {
  // 测试负数乘法
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, multiply(2, -4))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_function_edge_cases" {
  // 测试问候功能的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "overflow_protection_add" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
}

test "overflow_protection_multiply" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = multiply(length, width)
  assert_eq(50, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
}

test "string_operations_unicode" {
  // 测试字符串操作和Unicode支持
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, aBc123!", greet("aBc123"))
}