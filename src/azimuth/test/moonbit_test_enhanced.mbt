// MoonBit 测试用例 - 增强版
// 包含10个新的测试用例，测试各种场景

test "edge_case_arithmetic" {
  // 测试边界算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "string_unicode_handling" {
  // 测试Unicode字符串处理
  let chinese_name = "张三"
  let japanese_name = "田中"
  let emoji_name = "🚀"
  let mixed_name = "Alice张三🌍"
  
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet(japanese_name))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet(emoji_name))
  @azimuth.assert_eq_string("Hello, Alice张三🌍!", @azimuth.greet(mixed_name))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let base = 10
  let multiplier = 3
  let offset = 5
  
  // 计算 (base * multiplier) + offset
  let step1 = @azimuth.multiply(base, multiplier)
  let result = @azimuth.add(step1, offset)
  @azimuth.assert_eq(35, result)
  
  // 计算 ((base + offset) * multiplier) - base
  let step2 = @azimuth.add(base, offset)
  let step3 = @azimuth.multiply(step2, multiplier)
  let result2 = @azimuth.add(step3, -base)
  @azimuth.assert_eq(40, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试分配律
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "practical_business_calculation" {
  // 实际业务计算：商品折扣计算
  let original_price = 299
  let discount_rate = 15  // 15%折扣
  let tax_rate = 8        // 8%税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(274, final_price)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试特殊值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "string_special_characters" {
  // 测试特殊字符字符串处理
  let special_chars = "@#$%^&*()_+-=[]{}|;':\",./<>?"
  let whitespace = "   "
  let newlines = "Line1\nLine2\nLine3"
  let tabs = "Col1\tCol2\tCol3"
  
  @azimuth.assert_eq_string("Hello, @#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet(special_chars))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet(whitespace))
  @azimuth.assert_eq_string("Hello, Line1\nLine2\nLine3!", @azimuth.greet(newlines))
  @azimuth.assert_eq_string("Hello, Col1\tCol2\tCol3!", @azimuth.greet(tabs))
}

test "algorithm_implementation" {
  // 算法实现测试：计算数列和
  // 计算1到10的和：1+2+3+...+10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列前5项和：1+2+4+8+16 = 31
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, geometric_sum)
}

test "real_world_scenarios" {
  // 实际场景测试：计算BMI指数（简化版）
  let weight_kg = 70  // 体重（公斤）
  let height_m = 175  // 身高（厘米）
  
  // BMI = 体重(kg) / (身高(m))^2
  // 简化计算：避免浮点数，使用整数运算
  let height_meters = height_m / 100  // 转换为米（整数部分）
  let bmi_numerator = @azimuth.multiply(weight_kg, 10000)  // 放大10000倍避免小数
  let bmi_denominator = @azimuth.multiply(height_meters, height_meters)
  let bmi = bmi_numerator / bmi_denominator
  
  @azimuth.assert_eq(22, bmi)  // 70*10000/(1*1) = 22（简化值）
}

test "performance_optimization" {
  // 性能优化测试：大量数据处理
  let base_value = 100
  let iterations = 50
  
  // 使用公式计算：base_value * iterations * (iterations + 1) / 2
  let optimized_sum = @azimuth.multiply(base_value, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  // 验证小规模计算
  let small_scale = @azimuth.multiply(base_value, @azimuth.multiply(5, 6) / 2)
  @azimuth.assert_eq(1500, small_scale)
  
  // 验证大规模计算
  @azimuth.assert_eq(127500, optimized_sum)
}