// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法

test "add_with_large_numbers" {
  // 测试大数加法
  let num1 = 1000000
  let num2 = 2000000
  assert_eq(azimuth::add(num1, num2), 3000000)
}

test "multiply_with_negative_and_positive" {
  // 测试负数乘以正数
  assert_eq(azimuth::multiply(-5, 10), -50)
  assert_eq(azimuth::multiply(7, -3), -21)
}

test "greet_with_unicode_characters" {
  // 测试包含Unicode字符的问候
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("🌙"), "Hello, 🌙!")
}

test "add_sequence_operations" {
  // 测试连续加法操作
  let result = azimuth::add(azimuth::add(10, 20), azimuth::add(30, 40))
  assert_eq(result, 100)
}

test "multiply_by_one_and_zero" {
  // 测试乘以1和0的特殊情况
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(-15, 1), -15)
  assert_eq(azimuth::multiply(-15, 0), 0)
}

test "greet_with_whitespace" {
  // 测试包含空格的字符串
  assert_eq(azimuth::greet("John Doe"), "Hello, John Doe!")
  assert_eq(azimuth::greet("  leading"), "Hello,   leading!")
  assert_eq(azimuth::greet("trailing  "), "Hello, trailing  !")
}

test "mathematical_associativity" {
  // 测试结合律：(a + b) + c = a + (b + c)
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = azimuth::add(azimuth::add(a, b), c)
  let result2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "add_with_extreme_values" {
  // 测试极值加法
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(azimuth::add(max_int, -max_int), 0)
  assert_eq(azimuth::add(min_int, max_int), -1)
}

test "greet_with_special_characters" {
  // 测试特殊字符
  assert_eq(azimuth::greet("test@example.com"), "Hello, test@example.com!")
  assert_eq(azimuth::greet("user+tag@domain.org"), "Hello, user+tag@domain.org!")
}