// 用户添加的 MoonBit 测试用例
// 测试基本功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "zero_operations" {
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-9876, 0))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(-9876, @azimuth.add(-9876, 0))
}

test "identity_elements" {
  // 测试单位元
  let test_value = 42
  
  // 加法单位元
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法单位元
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-24, @azimuth.multiply(3, -8))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let result = @azimuth.add(product, -d)  // 80
  
  @azimuth.assert_eq(80, result)
}

test "string_special_characters" {
  // 测试特殊字符字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1000) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1000) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
}