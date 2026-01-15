// 额外的 MoonBit 测试用例
// 测试核心功能的各种场景和边界情况

test "arithmetics_series_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
}

test "geometric_progression" {
  // 测试等比数列：2 + 4 + 8 + 16 = 30
  let term1 = 2
  let term2 = multiply(term1, 2)
  let term3 = multiply(term2, 2)
  let term4 = multiply(term3, 2)
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(30, sum)
}

test "string_concatenation_with_numbers" {
  // 测试包含数字的字符串拼接
  let year = "2024"
  let month = "01"
  let day = "15"
  let date_string = year + "-" + month + "-" + day
  let result = greet(date_string)
  assert_eq_string("Hello, 2024-01-15!", result)
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let unit_price = 199
  let quantity = 5
  let discount_rate = 10  // 10%折扣
  let tax_rate = 8        // 8%税率
  let shipping = 25       // 运费
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算折扣
  let discount = multiply(subtotal, discount_rate) / 100
  // 计算折后价格
  let discounted_price = add(subtotal, -discount)
  // 计算税费
  let tax = multiply(discounted_price, tax_rate) / 100
  // 计算总价
  let total = add(add(discounted_price, tax), shipping)
  
  assert_eq(966, total)
}

test "temperature_conversion_formula" {
  // 测试温度转换公式：华氏度转摄氏度（简化版）
  // C = (F - 32) × 5/9，简化为 C = (F - 30) / 2
  let fahrenheit = 86
  let celsius = add(fahrenheit, -30) / 2
  assert_eq(28, celsius)
  
  // 反向验证：摄氏度转华氏度
  let back_to_fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(86, back_to_fahrenheit)
}

test "mathematical_identity_verification" {
  // 测试数学恒等式验证
  let x = 7
  let y = 13
  
  // 验证平方和公式：(x + y)² = x² + 2xy + y²
  let left_side = multiply(add(x, y), add(x, y))
  let right_side = add(add(multiply(x, x), multiply(2, multiply(x, y))), multiply(y, y))
  assert_eq(left_side, right_side)
  
  // 验证平方差公式：(x + y)(x - y) = x² - y²
  let diff_left = multiply(add(x, y), add(x, -y))
  let diff_right = add(multiply(x, x), multiply(-y, y))
  assert_eq(diff_left, diff_right)
}

test "edge_case_string_processing" {
  // 测试边缘情况的字符串处理
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含Unicode字符的字符串
  let unicode_string = "测试🌟MoonBit"
  let unicode_result = greet(unicode_string)
  assert_eq_string("Hello, 测试🌟MoonBit!", unicode_result)
}

test "performance_calculation_simulation" {
  // 测试性能计算模拟
  let base_score = 1000
  let performance_factor = 15  // 性能系数
  let bonus_multiplier = 3     // 奖励倍数
  
  // 计算基础得分
  let base_total = multiply(base_score, performance_factor)
  // 计算奖励得分
  let bonus_total = multiply(base_total, bonus_multiplier)
  // 计算最终得分
  let final_score = add(base_total, bonus_total)
  
  assert_eq(60000, final_score)
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法函数
  // 正常情况
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-2, divide_with_ceil(-7, 4))
}