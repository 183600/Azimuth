// 新的 MoonBit 测试用例
// 为 Azimuth 项目添加的标准测试用例

test "basic_arithmetic_commutativity" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
}

test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_edge_cases" {
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含数字和符号的字符串
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "complex Mathematical_calculations" {
  // 测试复合数学运算：(a + b) * (c - d)
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  
  let sum_ab = @azimuth.add(a, b)  // 20
  let diff_cd = @azimuth.add(c, -d)  // 15
  let result = @azimuth.multiply(sum_ab, diff_cd)  // 300
  
  @azimuth.assert_eq(300, result)
}

test "real_world_application_calculations" {
  // 测试实际应用：贷款月供计算（简化版）
  let loan_amount = 100000  // 贷款金额
  let interest_rate = 5     // 年利率百分比
  let loan_years = 20       // 贷款年限
  
  // 简化的月供计算：月供 = (本金 + 总利息) / (年数 × 12)
  let total_interest = @azimuth.multiply(loan_amount, @azimuth.multiply(interest_rate, loan_years)) / 100
  let total_payment = @azimuth.add(loan_amount, total_interest)
  let monthly_payment = total_payment / @azimuth.multiply(loan_years, 12)
  
  @azimuth.assert_eq(625, monthly_payment)  // 简化计算结果
}

test "performance_related_calculations" {
  // 测试性能计算：数据传输时间估算
  let data_size_mb = 100
  let speed_mbps = 20
  
  // 计算传输时间（秒）
  let transfer_time = data_size_mb / speed_mbps
  @azimuth.assert_eq(5, transfer_time)
  
  // 转换为分钟
  let transfer_minutes = transfer_time / 60
  @azimuth.assert_eq(0, transfer_minutes)  // 5秒 = 0分钟（整数除法）
}

test "scientific_calculations" {
  // 测试科学计算：简化的物理公式
  // 自由落体距离：d = g × t² / 2（简化为整数计算）
  let gravity = 10  // 简化的重力加速度
  let time = 3      // 时间（秒）
  
  let time_squared = @azimuth.multiply(time, time)  // 9
  let distance = @azimuth.multiply(gravity, time_squared) / 2  // 45
  
  @azimuth.assert_eq(45, distance)
}

test "business_calculations" {
  // 测试商业计算：利润和亏损分析
  let revenue = 50000
  let costs = 35000
  let tax_rate = 20  // 税率百分比
  
  // 计算税前利润
  let profit_before_tax = @azimuth.add(revenue, -costs)  // 15000
  
  // 计算税费
  let tax_amount = @azimuth.multiply(profit_before_tax, tax_rate) / 100  // 3000
  
  // 计算净利润
  let net_profit = @azimuth.add(profit_before_tax, -tax_amount)  // 12000
  
  @azimuth.assert_eq(15000, profit_before_tax)
  @azimuth.assert_eq(3000, tax_amount)
  @azimuth.assert_eq(12000, net_profit)
}

test "algorithmic_calculations" {
  // 测试算法计算：等差数列求和
  // 公式：sum = n × (first + last) / 2
  let n = 10           // 项数
  let first = 1        // 首项
  let last = 19        // 末项
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
  
  // 测试等比数列（简化）：2^4 = 16
  let base = 2
  let exponent = 4
  let power_result = @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(16, power_result)
}

test "error_validation_and_boundary_testing" {
  // 测试错误验证和边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, min_val) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 验证正常运算
  @azimuth.assert_true(@azimuth.add(100, 200) == 300)
  @azimuth.assert_true(@azimuth.multiply(15, 4) == 60)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}