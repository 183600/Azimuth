// Essential MoonBit 测试用例
// 这些测试用例涵盖了核心功能和实际应用场景

test "essential_basic_arithmetic" {
  // 基础算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "essential_string_greeting" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
}

test "essential_overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "essential_zero_handling" {
  // 零值处理测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "essential_negative_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "essential_complex_calculations" {
  // 复杂计算测试
  // 计算 (2 + 3) * 4 - 5
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(2, 3), 4), -5)
  @azimuth.assert_eq(15, result)
  
  // 计算平均数：(10 + 20 + 30) / 3
  let sum = @azimuth.add(@azimuth.add(10, 20), 30)
  let average = sum / 3
  @azimuth.assert_eq(20, average)
}

test "essential_practical_scenarios" {
  // 实际应用场景测试
  // 购物车计算：商品价格 × 数量
  let price = 199
  let quantity = 3
  let total = @azimuth.multiply(price, quantity)
  @azimuth.assert_eq(597, total)
  
  // 温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "essential_boundary_values" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  let one = 1
  let neg_one = -1
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, neg_one))
}

test "essential_assert_functions" {
  // 断言函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试条件表达式
  let condition = 5 > 3
  @azimuth.assert_true(condition)
  
  let condition2 = 2 > 10
  @azimuth.assert_false(condition2)
}

test "essential_string_edge_cases" {
  // 字符串边界情况测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
}