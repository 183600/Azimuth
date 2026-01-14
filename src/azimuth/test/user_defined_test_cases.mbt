// 用户定义的 MoonBit 测试用例
// 基于现有的 add, multiply, greet 等函数创建

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：1 + 4 + 7 + 10 + 13 = 35
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(35, sum)
}

test "geometric_sequence_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16 = 1024
  // 通过连续相乘实现
  let term1 = 2
  let term2 = multiply(term1, 2)  // 4
  let term3 = multiply(term2, 2)  // 8
  let term4 = multiply(term3, 2)  // 16
  let product = multiply(multiply(multiply(term1, term2), term3), term4)
  assert_eq(1024, product)
}

test "string_concatenation_with_numbers" {
  // 测试包含数字的字符串拼接
  let name_with_age = "Alice25"
  let result = greet(name_with_age)
  assert_eq_string("Hello, Alice25!", result)
  
  let name_with_symbols = "Bob_123!"
  let result2 = greet(name_with_symbols)
  assert_eq_string("Hello, Bob_123!", result2)
}

test "complex_calculations_with_negatives" {
  // 测试包含负数的复杂计算
  let a = -10
  let b = 20
  let c = -5
  let d = 15
  
  // 计算：(-10 + 20) × (-5 + 15) = 10 × 10 = 100
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let result = multiply(sum1, sum2)
  assert_eq(100, result)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值减1
  let max_minus_one = add(max_val, -1)
  assert_eq(2147483646, max_minus_one)
  
  // 测试最小值加1
  let min_plus_one = add(min_val, 1)
  assert_eq(-2147483647, min_plus_one)
  
  // 测试最大值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(0, multiply(max_val, 0))
}

test "real_world_price_calculation" {
  // 实际商品价格计算
  let unit_price = 199
  let quantity = 4
  let discount = 50  // 总折扣
  let tax_rate = 8   // 税率百分比
  
  // 计算总价
  let subtotal = multiply(unit_price, quantity)
  // 应用折扣
  let discounted = add(subtotal, -discount)
  // 计算税费
  let tax = multiply(discounted, tax_rate) / 100
  // 最终价格
  let final_price = add(discounted, tax)
  
  assert_eq(798, final_price)  // (199×4 - 50) + (199×4 - 50)×8/100 = 748 + 59.84 ≈ 798
}

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 12
  let y = 8
  
  // 验证平方差公式：(x+y)(x-y) = x²-y²
  let sum = add(x, y)
  let diff = add(x, -y)
  let left_side = multiply(sum, diff)
  
  let x_squared = multiply(x, x)
  let y_squared = multiply(y, y)
  let right_side = add(x_squared, multiply(-1, y_squared))
  
  assert_eq(left_side, right_side)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度：F = C × 2 + 30 (简化版)
  
  // 测试冰点
  let celsius_freezing = 0
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  assert_eq(30, fahrenheit_freezing)
  
  // 测试室温
  let celsius_room = 20
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  assert_eq(70, fahrenheit_room)
  
  // 测试沸点
  let celsius_boiling = 100
  let fahrenheit_boiling = add(multiply(celsius_boiling, 2), 30)
  assert_eq(230, fahrenheit_boiling)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试各种特殊字符组合
  let unicode_name = "张三李四"
  assert_eq_string("Hello, 张三李四!", greet(unicode_name))
  
  let mixed_symbols = "@#$%^&*()"
  assert_eq_string("Hello, @#$%^&*()!", greet(mixed_symbols))
  
  let numeric_string = "1234567890"
  assert_eq_string("Hello, 1234567890!", greet(numeric_string))
  
  let whitespace_name = "  spaced  "
  assert_eq_string("Hello,   spaced  !", greet(whitespace_name))
}

test "compound_interest_calculation" {
  // 复利计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率×年数)
  let growth_factor = add(1, multiply(rate_percent, years))
  let final_amount = multiply(principal, growth_factor)
  
  assert_eq(160000, final_amount)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}