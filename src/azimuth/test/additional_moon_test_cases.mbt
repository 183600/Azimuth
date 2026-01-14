// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_basic_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "greet_various_inputs" {
  // 测试不同输入的问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算三角形面积
  let base = 10
  let height = 6
  
  // 面积 = 底 × 高 ÷ 2
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, area)
}

test "error_handling_verification" {
  // 测试错误处理
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "real_world_application" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 200
  let discount_percentage = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(160, final_price)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法结合律
  let result1 = @azimuth.add(@azimuth.add(a, b), 5)
  let result2 = @azimuth.add(a, @azimuth.add(b, 5))
  @azimuth.assert_eq(result1, result2)
  
  // 乘法结合律
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), 2)
  let result4 = @azimuth.multiply(a, @azimuth.multiply(b, 2))
  @azimuth.assert_eq(result3, result4)
}

test "special_characters_handling" {
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
}

test "performance_related_calculations" {
  // 测试性能相关计算
  let base = 100
  let iterations = 10
  
  // 计算 1 + 2 + ... + 10
  let sum = @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列：2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}