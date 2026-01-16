// Azimuth 项目附加标准测试用例
// 使用标准 MoonBit 测试语法

test "add_with_floating_point_conversion" {
  // 测试整数加法的精度
  let result1 = azimuth::add(1000, 2000)
  assert_eq(result1, 3000)
  
  let result2 = azimuth::add(-500, 250)
  assert_eq(result2, -250)
}

test "multiply_by_negative_one" {
  // 测试乘以-1的符号变化
  let positive = 42
  let negative = -42
  
  let result1 = azimuth::multiply(positive, -1)
  assert_eq(result1, -42)
  
  let result2 = azimuth::multiply(negative, -1)
  assert_eq(result2, 42)
}

test "greet_with_whitespace" {
  // 测试包含空格的字符串
  let result1 = azimuth::greet("John Doe")
  assert_eq(result1, "Hello, John Doe!")
  
  let result2 = azimuth::greet("  spaced  ")
  assert_eq(result2, "Hello,   spaced  !")
}

test "sequential_operations" {
  // 测试连续运算
  let start = 10
  let step1 = azimuth::add(start, 5)  // 15
  let step2 = azimuth::multiply(step1, 2)  // 30
  let step3 = azimuth::add(step2, -10)  // 20
  
  assert_eq(step3, 20)
}

test "identity_operations" {
  // 测试单位元运算
  let value = 123
  
  // 加法单位元
  let add_identity = azimuth::add(value, 0)
  assert_eq(add_identity, value)
  
  // 乘法单位元
  let mult_identity = azimuth::multiply(value, 1)
  assert_eq(mult_identity, value)
}

test "string_length_cases" {
  // 测试不同长度的字符串
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a very long string for testing purposes"
  
  let result1 = azimuth::greet(short)
  assert_eq(result1, "Hello, Hi!")
  
  let result2 = azimuth::greet(medium)
  assert_eq(result2, "Hello, MoonBit!")
  
  let result3 = azimuth::greet(long)
  assert_eq(result3, "Hello, This is a very long string for testing purposes!")
}

test "negative_zero_operations" {
  // 测试负零相关运算
  let positive = 15
  let negative = -15
  
  let result1 = azimuth::add(positive, -15)
  assert_eq(result1, 0)
  
  let result2 = azimuth::add(negative, 15)
  assert_eq(result2, 0)
}

test "multiply_by_zero_variations" {
  // 测试各种零的乘法
  let positive = 100
  let negative = -100
  
  let result1 = azimuth::multiply(positive, 0)
  assert_eq(result1, 0)
  
  let result2 = azimuth::multiply(negative, 0)
  assert_eq(result2, 0)
  
  let result3 = azimuth::multiply(0, 0)
  assert_eq(result3, 0)
}

test "greet_with_special_characters" {
  // 测试特殊字符处理
  let result1 = azimuth::greet("Café")
  assert_eq(result1, "Hello, Café!")
  
  let result2 = azimuth::greet("测试")
  assert_eq(result2, "Hello, 测试!")
  
  let result3 = azimuth::greet("🌙")
  assert_eq(result3, "Hello, 🌙!")
}

test "additive_inverse_property" {
  // 测试加法逆元性质
  let values = [0, 1, -1, 42, -100, 999999]
  
  for value in values {
    let inverse = azimuth::multiply(value, -1)
    let result = azimuth::add(value, inverse)
    assert_eq(result, 0)
  }
}