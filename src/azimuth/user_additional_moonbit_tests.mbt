// 用户添加的 MoonBit 测试用例
// 这些测试用例涵盖了各种场景，包括基本功能、边界条件和实际应用

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
}

test "multiply_by_negative" {
  // 测试与负数相乘
  assert_eq(-15, multiply(3, -5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(0, -10))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, Test2023!", greet("Test2023"))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(100000000, add(50000000, 50000000))
}

test "multiply_with_large_numbers" {
  // 测试大数相乘
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(25000000, multiply(5000, 5000))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%", greet("@#$%"))
  assert_eq_string("Hello, []{}!", greet("[]{}"))
}

test "add_sequence" {
  // 测试序列相加
  let result = add(add(1, 2), add(3, 4))
  assert_eq(10, result)
}

test "multiply_sequence" {
  // 测试序列相乘
  let result = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result)
}

test "complex_calculation" {
  // 测试复合计算：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(22, result)
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
}