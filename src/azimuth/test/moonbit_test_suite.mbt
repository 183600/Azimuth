// MoonBit 测试套件
// 包含 10 个标准测试用例，涵盖各种测试场景

test "boundary_arithmetic_operations" {
  // 边界算术运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_series_calculations" {
  // 数学数列计算测试
  
  // 计算等差数列前n项和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列前n项和：1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, geo_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", @azimuth.greet(long_string))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试包含数字的字符串
  let numeric_string = "1234567890"
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet(numeric_string))
}

test "real_world_financial_calculations" {
  // 实际金融计算测试
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  @azimuth.assert_eq(1500, simple_interest)
  
  // 复利计算（简化版）：本金 × (1 + 利率)^时间
  let compound_factor = @azimuth.add(100, @azimuth.multiply(rate, time))
  let compound_amount = @azimuth.multiply(principal, compound_factor) / 100
  @azimuth.assert_eq(115000, compound_amount)
  
  // 贷款月供计算（简化版）：总贷款 / (年数 × 12)
  let loan_amount = 120000
  let loan_years = 10
  let monthly_payment = loan_amount / @azimuth.multiply(loan_years, 12)
  @azimuth.assert_eq(1000, monthly_payment)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  let overflow_result = @azimuth.add(max_val, 100)
  @azimuth.assert_true(overflow_result == max_val)
  
  // 测试数学运算验证
  let a = 10
  let b = 20
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "performance_related_operations" {
  // 性能相关操作测试
  
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_sum = @azimuth.add(large_num1, large_num2)
  let large_product = @azimuth.multiply(large_num1, 100)
  
  @azimuth.assert_eq(3000000, large_sum)
  @azimuth.assert_eq(100000000, large_product)
  
  // 批量计算模拟
  let base = 100
  let multiplier = 50
  let batch_result = @azimuth.multiply(base, multiplier)
  @azimuth.assert_eq(5000, batch_result)
  
  // 累积计算模拟
  let cumulative = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, cumulative)
}

test "complex_algorithm_simulation" {
  // 复杂算法模拟测试
  
  // 模拟阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 模拟幂运算：2^8 = 256
  let power_2_8 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(256, power_2_8)
  
  // 模拟斐波那契数列：F(5) = F(4) + F(3) = 3 + 2 = 5
  let fib_5 = @azimuth.add(@azimuth.add(@azimuth.add(1, 1), 2), 3)
  @azimuth.assert_eq(5, fib_5)
}

test "data_conversion_and_formatting" {
  // 数据转换和格式化测试
  
  // 模拟数字到字符串的转换（通过greet函数）
  let year = "2023"
  let greeting = @azimuth.greet(year)
  @azimuth.assert_eq_string("Hello, 2023!", greeting)
  
  // 模拟布尔值到字符串的转换
  let true_str = "true"
  let false_str = "false"
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_str))
  
  // 模拟浮点数的简化表示
  let pi_approx = "3.14"
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet(pi_approx))
  
  // 模拟日期格式
  let date_str = "2023-12-25"
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet(date_str))
}

test "conditional_logic_scenarios" {
  // 条件逻辑场景测试
  
  let x = 15
  let y = 20
  let z = 25
  
  // 条件计算：如果 x < y，则计算 x + y，否则计算 x * y
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result1)
  
  // 条件计算：如果 y > z，则计算 y - z，否则计算 z - y
  let condition2 = y > z
  let result2 = if condition2 { @azimuth.add(y, -z) } else { @azimuth.add(z, -y) }
  @azimuth.assert_eq(5, result2)
  
  // 嵌套条件：比较三个数的大小
  let max = if x > y { if x > z { x } else { z } } else { if y > z { y } else { z } }
  @azimuth.assert_eq(25, max)
}

test "integrated_application_scenarios" {
  // 综合应用场景测试
  
  // 场景1：计算订单总价
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  let subtotal = @azimuth.multiply(item_price, quantity)
  let discounted_total = @azimuth.add(subtotal, -discount)
  let final_total = @azimuth.add(discounted_total, shipping)
  @azimuth.assert_eq(602, final_total)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  let score5 = 88
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average_score = total_score / 5
  @azimuth.assert_eq(86, average_score)
  
  // 场景3：计算矩形周长和面积
  let length = 10
  let width = 6
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(32, perimeter)
  @azimuth.assert_eq(60, area)
}