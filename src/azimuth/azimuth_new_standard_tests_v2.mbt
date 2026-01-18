// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  let result = add(2, 3)
  assert_eq(5, result)
}

test "add_function_with_negative" {
  // 测试负数加法
  let result = add(-5, 3)
  assert_eq(-2, result)
}

test "add_function_with_zero" {
  // 测试与零相加
  let result1 = add(10, 0)
  let result2 = add(0, 10)
  assert_eq(10, result1)
  assert_eq(10, result2)
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  let result = multiply(4, 5)
  assert_eq(20, result)
}

test "multiply_function_with_negative" {
  // 测试负数乘法
  let result1 = multiply(-3, 4)
  let result2 = multiply(-3, -4)
  assert_eq(-12, result1)
  assert_eq(12, result2)
}

test "multiply_function_with_zero" {
  // 测试与零相乘
  let result1 = multiply(10, 0)
  let result2 = multiply(0, 10)
  assert_eq(0, result1)
  assert_eq(0, result2)
}

test "greet_function_basic" {
  // 测试基本问候功能
  let result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", result)
}

test "greet_function_with_empty" {
  // 测试空字符串问候
  let result = greet("")
  assert_eq_string("Hello, !", result)
}

test "greet_function_with_special_chars" {
  // 测试特殊字符问候
  let result = greet("测试@123")
  assert_eq_string("Hello, 测试@123!", result)
}