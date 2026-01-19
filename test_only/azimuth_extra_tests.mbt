// 额外的 MoonBit 标准测试用例
// 测试 azimuth 项目的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数的交换律
  let c = -123
  let d = 456
  assert_eq(add(c, d), add(d, c))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "greet_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌎!", greet("🌎"))
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 边界值的自运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "calculation_precision_validation" {
  // 测试计算精度验证
  // 复杂的多步计算
  let initial = 100
  let step1 = add(initial, multiply(2, 15))  // 100 + 30 = 130
  let step2 = multiply(step1, 3)             // 130 * 3 = 390
  let step3 = add(step2, -50)                // 390 - 50 = 340
  let final_result = divide_with_ceil(step3, 8)  // ceil(340/8) = 43
  
  assert_eq(130, step1)
  assert_eq(390, step2)
  assert_eq(340, step3)
  assert_eq(43, final_result)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 银行存款利息计算：本金 × (1 + 年利率 × 存款年限)
  let principal = 10000  // 本金 10000 元
  let annual_rate = 3    // 年利率 3%
  let years = 5          // 存款 5 年
  
  let interest_rate = multiply(annual_rate, years)  // 3% × 5 = 15%
  let total_amount = multiply(principal, add(100, interest_rate))  // 10000 × 115% = 11500
  
  assert_eq(150000, total_amount)  // 10000 × 115 = 1150000 (表示 11500 元)
}

test "string_boundary_conditions" {
  // 测试字符串边界条件
  // 测试各种特殊字符组合
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, mixed'quote\"test!", greet("mixed'quote\"test"))
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 计算等差数列：3, 7, 11, 15, 19 的和
  // 公式：n/2 × (首项 + 末项)
  let n = 5
  let first_term = 3
  let last_term = 19
  
  let series_sum = multiply(n, add(first_term, last_term)) / 2
  assert_eq(55, series_sum)  // 5/2 × (3 + 19) = 55
  
  // 验证手动计算
  let manual_sum = add(add(add(add(3, 7), 11), 15), 19)
  assert_eq(55, manual_sum)
}

test "performance_stress_test" {
  // 测试性能压力场景
  // 大量重复计算的累积
  let base = 123
  let multiplier = 456
  let iterations = 10
  
  // 计算 base × multiplier × iterations
  let single_product = multiply(base, multiplier)
  let total_product = multiply(single_product, iterations)
  
  assert_eq(56088, single_product)  // 123 × 456
  assert_eq(560880, total_product)  // 56088 × 10
}

test "comprehensive_assert_validation" {
  // 综合断言验证测试
  let test_value = 42
  let test_string = "MoonBit"
  let bool_condition = test_value > 0
  
  // 测试所有断言函数
  assert_eq(test_value, 42)
  assert_eq_string(test_string, "MoonBit")
  assert_true(bool_condition)
  assert_false(test_value < 0)
  
  // 测试计算结果的断言
  let calc_result = add(multiply(test_value, 2), 16)
  assert_eq(100, calc_result)
  
  // 测试字符串拼接的断言
  let greeting_result = greet(test_string)
  assert_eq_string("Hello, MoonBit!", greeting_result)
}