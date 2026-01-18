// Azimuth 综合测试套件
// 标准 MoonBit 测试语法，覆盖核心功能

test "add_positive_numbers" {
  // 测试正数加法
  let result = add(15, 27)
  assert_eq(42, result)
}

test "add_negative_numbers" {
  // 测试负数加法
  let result = add(-13, -8)
  assert_eq(-21, result)
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  let result = add(20, -7)
  assert_eq(13, result)
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  let result = multiply(6, 9)
  assert_eq(54, result)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  let result1 = multiply(-4, 7)
  let result2 = multiply(-4, -7)
  assert_eq(-28, result1)
  assert_eq(28, result2)
}

test "multiply_by_zero" {
  // 测试乘以零
  let result = multiply(123, 0)
  assert_eq(0, result)
}

test "greet_standard_name" {
  // 测试标准名称问候
  let result = greet("Alice")
  assert_eq_string("Hello, Alice!", result)
}

test "greet_numeric_string" {
  // 测试数字字符串问候
  let result = greet("12345")
  assert_eq_string("Hello, 12345!", result)
}

test "greet_unicode_name" {
  // 测试 Unicode 名称问候
  let result = greet("张三")
  assert_eq_string("Hello, 张三!", result)
}