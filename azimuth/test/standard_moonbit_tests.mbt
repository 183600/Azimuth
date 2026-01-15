// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "string_concatenation_with_spaces" {
  // 测试包含空格的字符串拼接
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "add_with_large_positive_numbers" {
  // 测试大正数相加
  let num1 = 1000000
  let num2 = 2000000
  let result = @azimuth.add(num1, num2)
  @azimuth.assert_eq(3000000, result)
}

test "add_with_large_negative_numbers" {
  // 测试大负数相加
  let num1 = -1000000
  let num2 = -2000000
  let result = @azimuth.add(num1, num2)
  @azimuth.assert_eq(-3000000, result)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let num1 = 10000
  let num2 = 1000
  let result = @azimuth.multiply(num1, num2)
  @azimuth.assert_eq(10000000, result)
}

test "multiply_negative_by_positive" {
  // 测试负数乘以正数
  let result = @azimuth.multiply(-15, 4)
  @azimuth.assert_eq(-60, result)
}

test "multiply_both_negative" {
  // 测试两个负数相乘
  let result = @azimuth.multiply(-12, -8)
  @azimuth.assert_eq(96, result)
}

test "add_mixed_sign_numbers" {
  // 测试不同符号的数相加
  let result1 = @azimuth.add(1000, -500)
  let result2 = @azimuth.add(-1000, 500)
  @azimuth.assert_eq(500, result1)
  @azimuth.assert_eq(-500, result2)
}

test "mathematical_property_distributive" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "greet_with_unicode_characters" {
  // 测试Unicode字符处理
  let result = @azimuth.greet("🚀")
  @azimuth.assert_eq_string("Hello, 🚀!", result)
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  // 计算: (a * b) + (c * d) - e
  let a = 15
  let b = 4
  let c = 8
  let d = 3
  let e = 20
  
  let step1 = @azimuth.multiply(a, b)  // 15 * 4 = 60
  let step2 = @azimuth.multiply(c, d)  // 8 * 3 = 24
  let step3 = @azimuth.add(step1, step2)  // 60 + 24 = 84
  let result = @azimuth.add(step3, -e)  // 84 + (-20) = 64
  
  @azimuth.assert_eq(64, result)
}