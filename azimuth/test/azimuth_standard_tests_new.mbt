// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖主要功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.add(-2, 1), -1)
  @azimuth.assert_eq(@azimuth.add(5, 5), 10)
}

test "add_with_zero" {
  // 测试与零相加的情况
  @azimuth.assert_eq(@azimuth.add(0, 7), 7)
  @azimuth.assert_eq(@azimuth.add(7, 0), 7)
  @azimuth.assert_eq(@azimuth.add(0, -3), -3)
  @azimuth.assert_eq(@azimuth.add(-3, 0), -3)
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(max_val, max_val), max_val)
  
  // 测试负数溢出
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val, min_val), min_val)
  
  // 测试最小值特殊情况
  @azimuth.assert_eq(@azimuth.add(min_val, -100), min_val)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.multiply(-2, 3), -6)
  @azimuth.assert_eq(@azimuth.multiply(3, 5), 15)
}

test "multiply_with_special_values" {
  // 测试特殊值乘法
  @azimuth.assert_eq(@azimuth.multiply(1, 7), 7)
  @azimuth.assert_eq(@azimuth.multiply(7, 1), 7)
  @azimuth.assert_eq(@azimuth.multiply(-1, 7), -7)
  @azimuth.assert_eq(@azimuth.multiply(1, -7), -7)
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)
  
  // 测试负数溢出
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 2), min_val)
  
  // 测试最小值特殊情况
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("Alice"), "Hello, Alice!")
  @azimuth.assert_eq_string(@azimuth.greet("Bob"), "Hello, Bob!")
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("🌍"), "Hello, 🌍!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 5
  let c = 3
  
  // 测试 (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result1, 45)
  
  // 测试 a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(result2, 53)
  
  // 测试 (a - b) * c
  let result3 = @azimuth.multiply(@azimuth.add(a, -b), c)
  @azimuth.assert_eq(result3, 15)
}