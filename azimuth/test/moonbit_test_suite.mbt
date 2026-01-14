// MoonBit 测试套件
// 包含10个标准测试用例，测试 azimuth 包的核心功能

test "basic_addition" {
  // 测试基本加法运算
  let result = @azimuth.add(5, 3)
  @azimuth.assert_eq(8, result)
}

test "addition_with_zero" {
  // 测试与0相加
  let result1 = @azimuth.add(0, 10)
  let result2 = @azimuth.add(10, 0)
  let result3 = @azimuth.add(0, 0)
  @azimuth.assert_eq(10, result1)
  @azimuth.assert_eq(10, result2)
  @azimuth.assert_eq(0, result3)
}

test "addition_with_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(-5, 3)
  let result2 = @azimuth.add(5, -3)
  let result3 = @azimuth.add(-5, -3)
  @azimuth.assert_eq(-2, result1)
  @azimuth.assert_eq(2, result2)
  @azimuth.assert_eq(-8, result3)
}

test "addition_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试负溢出
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
}

test "basic_multiplication" {
  // 测试基本乘法运算
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "multiplication_with_zero_and_one" {
  // 测试与0和1相乘
  let result1 = @azimuth.multiply(10, 0)
  let result2 = @azimuth.multiply(0, 10)
  let result3 = @azimuth.multiply(10, 1)
  let result4 = @azimuth.multiply(1, 10)
  @azimuth.assert_eq(0, result1)
  @azimuth.assert_eq(0, result2)
  @azimuth.assert_eq(10, result3)
  @azimuth.assert_eq(10, result4)
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  let result1 = @azimuth.multiply(-4, 5)
  let result2 = @azimuth.multiply(4, -5)
  let result3 = @azimuth.multiply(-4, -5)
  @azimuth.assert_eq(-20, result1)
  @azimuth.assert_eq(-20, result2)
  @azimuth.assert_eq(20, result3)
}

test "multiplication_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  let result1 = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试最小值乘法
  let result2 = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, result2)
}

test "greet_function_basic" {
  // 测试基本问候功能
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = @azimuth.greet("")
  let result2 = @azimuth.greet("123")
  let result3 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, !", result1)
  @azimuth.assert_eq_string("Hello, 123!", result2)
  @azimuth.assert_eq_string("Hello, MoonBit!", result3)
}