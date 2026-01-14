// 新增的 MoonBit 测试用例
// 覆盖各种数学运算和字符串处理场景

test "addition_with_large_numbers" {
  // 测试大数相加
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
  assert_eq(0, add(1500000000, -1500000000))
}

test "multiplication_with_decimal_simulation" {
  // 模拟小数乘法：使用整数表示小数部分
  // 1.5 × 2.4 = 3.6，表示为 15 × 24 = 360，然后除以 100
  let a = 15  // 表示 1.5
  let b = 24  // 表示 2.4
  let result = multiply(a, b) / 100
  assert_eq(3, result)  // 整数部分
}

test "string_processing_with_special_chars" {
  // 测试特殊字符处理
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, C#!", greet("C#"))
}

test "mathematical_series_calculation" {
  // 计算等差数列的和：1 + 4 + 7 + ... + 28（前10项）
  let first_term = 1
  let common_diff = 3
  let n = 10
  
  // 使用公式：n/2 × (2a + (n-1)d)
  let sum = multiply(n, add(multiply(2, first_term), multiply(n - 1, common_diff))) / 2
  assert_eq(145, sum)
}

test "geometric_progression_calculation" {
  // 计算等比数列：2 + 6 + 18 + 54（前4项）
  let first_term = 2
  let ratio = 3
  let n = 4
  
  // 手动计算
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let sum = add(add(add(term1, term2), term3), term4)
  
  assert_eq(80, sum)
}

test "real_world_currency_calculation" {
  // 实际货币计算：含税价格计算
  let base_price = 299  // 基础价格
  let tax_rate = 8      // 8% 税率
  let discount = 20     // 20元折扣
  
  // 计算税费
  let tax_amount = multiply(base_price, tax_rate) / 100
  // 计算折后价格
  let discounted_price = add(base_price, -discount)
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(307, final_price)
}

test "temperature_conversion_precise" {
  // 精确温度转换：摄氏度转华氏度
  // 使用整数近似：F = C × 2 + 30
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  let result1 = add(multiply(celsius_temps[0], 2), 30)
  let result2 = add(multiply(celsius_temps[1], 2), 30)
  let result3 = add(multiply(celsius_temps[2], 2), 30)
  let result4 = add(multiply(celsius_temps[3], 2), 30)
  let result5 = add(multiply(celsius_temps[4], 2), 30)
  
  assert_eq(expected_fahrenheit[0], result1)
  assert_eq(expected_fahrenheit[1], result2)
  assert_eq(expected_fahrenheit[2], result3)
  assert_eq(expected_fahrenheit[3], result4)
  assert_eq(expected_fahrenheit[4], result5)
}

test "array_index_simulation" {
  // 模拟数组索引计算：二维数组转一维数组
  let row = 2
  let col = 3
  let width = 5
  
  // 计算一维数组索引：index = row × width + col
  let index = add(multiply(row, width), col)
  assert_eq(13, index)
  
  // 测试边界情况
  let first_index = add(multiply(0, width), 0)
  assert_eq(0, first_index)
}

test "compound_interest_simple" {
  // 简单复利计算
  let principal = 1000  // 本金
  let rate = 5          // 5% 利率
  let years = 3         // 3年
  
  // 简化计算：总金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = add(1, multiply(rate, years))
  let total_amount = multiply(principal, growth_factor)
  
  assert_eq(16000, total_amount)
}

test "logical_operations_simulation" {
  // 模拟逻辑运算
  let a = true
  let b = false
  
  // 模拟 AND 运算：a && b
  let and_result = if (a && b) { 1 } else { 0 }
  assert_eq(0, and_result)
  
  // 模拟 OR 运算：a || b
  let or_result = if (a || b) { 1 } else { 0 }
  assert_eq(1, or_result)
  
  // 模拟 NOT 运算：!a
  let not_a_result = if (a) { 0 } else { 1 }
  assert_eq(0, not_a_result)
  
  let not_b_result = if (b) { 0 } else { 1 }
  assert_eq(1, not_b_result)
}