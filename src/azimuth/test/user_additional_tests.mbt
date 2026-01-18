// 用户添加的额外测试用例
// 测试 Azimuth 项目的各种功能

test "addition_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

test "multiplication_identity_elements" {
  // 测试乘法单位元
  let test_value = 42
  
  // 乘以1应该等于原值
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
  
  // 乘以0应该等于0
  @azimuth.assert_eq(0, @azimuth.multiply(test_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_value))
  
  // 测试负数情况
  let negative_value = -42
  @azimuth.assert_eq(negative_value, @azimuth.multiply(negative_value, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(negative_value, 0))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, New\nLine!", @azimuth.greet("New\nLine"))
  @azimuth.assert_eq_string("Hello, Tab\tCharacter!", @azimuth.greet("Tab\tCharacter"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  @azimuth.assert_eq_string("Hello, Version1.0!", @azimuth.greet("Version1.0"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Leading Space!", @azimuth.greet(" Leading Space"))
  @azimuth.assert_eq_string("Hello, Trailing Space !", @azimuth.greet("Trailing Space "))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  // 计算等差数列的和: 3, 7, 11, 15, 19 (公差为4)
  let first_term = 3
  let common_diff = 4
  let num_terms = 5
  
  // 手动计算各项
  let term1 = first_term
  let term2 = @azimuth.add(term1, common_diff)
  let term3 = @azimuth.add(term2, common_diff)
  let term4 = @azimuth.add(term3, common_diff)
  let term5 = @azimuth.add(term4, common_diff)
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  // 验证结果: 3 + 7 + 11 + 15 + 19 = 55
  @azimuth.assert_eq(55, sum)
  
  // 使用公式验证: n × (首项 + 末项) / 2
  let formula_sum = @azimuth.multiply(num_terms, @azimuth.add(first_term, term5)) / 2
  @azimuth.assert_eq(sum, formula_sum)
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与正数相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))  // 应该溢出并限制在最大值
  
  // 最小值与负数相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况: -2147483648 × -1 = -2147483648
  
  // 测试接近边界的值
  let near_max = 2147483640
  let near_min = -2147483640
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(near_max, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(near_min, 2))
}

test "geometric_progression_calculation" {
  // 测试等比数列计算
  // 计算等比数列: 2, 6, 18, 54, 162 (公比为3)
  let first_term = 2
  let common_ratio = 3
  let num_terms = 5
  
  // 手动计算各项
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, common_ratio)
  let term3 = @azimuth.multiply(term2, common_ratio)
  let term4 = @azimuth.multiply(term3, common_ratio)
  let term5 = @azimuth.multiply(term4, common_ratio)
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  // 验证结果: 2 + 6 + 18 + 54 + 162 = 242
  @azimuth.assert_eq(242, sum)
  
  // 验证最后一项: 2 × 3^4 = 162
  @azimuth.assert_eq(162, term5)
}

test "mixed_operations_evaluation" {
  // 测试混合运算求值
  // 计算: (a + b) × c - d ÷ e (简化为整数运算)
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  let e = 5
  
  // 计算括号内: a + b = 25
  let parentheses = @azimuth.add(a, b)
  
  // 计算乘法: 25 × 4 = 100
  let multiplication = @azimuth.multiply(parentheses, c)
  
  // 计算除法: 20 ÷ 5 = 4
  let division = d / e
  
  // 计算最终结果: 100 - 4 = 96
  let result = @azimuth.add(multiplication, -division)
  
  @azimuth.assert_eq(96, result)
}

test "financial_loan_calculation" {
  // 测试金融贷款计算
  // 简化的贷款计算: 月还款额 = (本金 + 利息) ÷ 期数
  let principal = 120000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 5           // 贷款年限
  let months_per_year = 12
  
  // 计算总利息: 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years)
  
  // 计算总还款额: 本金 + 总利息
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 计算总月数
  let total_months = @azimuth.multiply(years, months_per_year)
  
  // 计算月还款额
  let monthly_payment = total_payment / total_months
  
  // 验证结果: (120000 + 120000×6×5) ÷ 60 = 72000 ÷ 60 = 12000
  @azimuth.assert_eq(12000, monthly_payment)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度: F = C × 9/5 + 32 (简化为整数: F = C × 2 + 30)
  
  // 测试常见温度点
  let test_temperatures = [(0, 30), (10, 50), (20, 70), (30, 90), (40, 110)]
  
  // 测试每个温度点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))  // 0°C → 30°F
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))  // 10°C → 50°F
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))  // 20°C → 70°F
  @azimuth.assert_eq(90, @azimuth.add(@azimuth.multiply(30, 2), 30))  // 30°C → 90°F
  @azimuth.assert_eq(110, @azimuth.add(@azimuth.multiply(40, 2), 30))  // 40°C → 110°F
  
  // 测试极端温度
  @azimuth.assert_eq(-70, @azimuth.add(@azimuth.multiply(-50, 2), 30))  // -50°C → -70°F
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))  // 100°C → 230°F
}

test "assert_functions_validation" {
  // 测试断言函数本身
  // 测试 assert_true
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  @azimuth.assert_true(@azimuth.multiply(4, 5) == 20)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  // 测试 assert_false
  @azimuth.assert_false(@azimuth.add(2, 3) == 6)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 21)
  @azimuth.assert_false(@azimuth.greet("Test") == "Hi, Test!")
  
  // 测试 assert_eq
  @azimuth.assert_eq(42, @azimuth.add(20, 22))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}