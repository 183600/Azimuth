// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let c = -789
  let d = 321
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiply_associative_property" {
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let a = 12
  let b = 15
  let c = 20
  
  let left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left, right)
}

test "greet_with_various_input_types" {
  // 测试 greet 函数处理不同类型的输入
  @azimuth.assert_eq_string("Hello, JohnDoe!", @azimuth.greet("JohnDoe"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, 特殊字符!", @azimuth.greet("特殊字符"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算折扣价格
  let original_price = 1000
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(918, final_price)  // 1000 - 150 + 68 = 918
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算：计算等差数列的和
  // 1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
}

test "real_world_application_test" {
  // 测试实际应用场景：计算BMI（身体质量指数）
  // BMI = 体重(kg) / 身高(m)^2
  // 使用整数计算简化：BMI指数 = 体重(kg) × 10000 / 身高(cm)^2
  let weight_kg = 70
  let height_cm = 175
  
  let height_squared = @azimuth.multiply(height_cm, height_cm)
  let bmi_index = @azimuth.multiply(weight_kg, 10000) / height_squared
  
  @azimuth.assert_eq(22, bmi_index)  // 70 × 10000 / 175^2 ≈ 22.85，取整为22
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界情况的正确处理
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}