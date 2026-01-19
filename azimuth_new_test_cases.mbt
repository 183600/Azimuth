// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "string_greeting_with_unicode" {
  // 测试包含Unicode字符的问候功能
  let result1 = azimuth.greet("张三")
  @assertion.assert_eq_string("Hello, 张三!", result1)?
  
  let result2 = azimuth.greet("José")
  @assertion.assert_eq_string("Hello, José!", result2)?
  
  let result3 = azimuth.greet("🌟Star🌟")
  @assertion.assert_eq_string("Hello, 🌟Star🌟!", result3)?
}

test "addition_with_negative_numbers" {
  // 测试负数加法运算
  @assertion.assert_eq(-10, azimuth.add(-5, -5))?
  @assertion.assert_eq(0, azimuth.add(-10, 10))?
  @assertion.assert_eq(5, azimuth.add(-3, 8))?
  @assertion.assert_eq(-15, azimuth.add(-20, 5))?
}

test "multiplication_with_mixed_signs" {
  // 测试混合符号乘法运算
  @assertion.assert_eq(-15, azimuth.multiply(-3, 5))?
  @assertion.assert_eq(20, azimuth.multiply(-4, -5))?
  @assertion.assert_eq(0, azimuth.multiply(0, -100))?
  @assertion.assert_eq(-100, azimuth.multiply(10, -10))?
}

test "compound_mathematical_operations" {
  // 测试复合数学运算
  let a = 10
  let b = 5
  let c = 2
  
  // 计算 (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  @assertion.assert_eq(30, result1)?
  
  // 计算 a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  @assertion.assert_eq(52, result2)?
  
  // 计算 (a - b) * (b + c)
  let result3 = azimuth.multiply(azimuth.add(a, -b), azimuth.add(b, c))
  @assertion.assert_eq(35, result3)?
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 计算简单利息：本金 * 利率 * 年数 / 100
  let simple_interest = azimuth.multiply(azimuth.multiply(principal, interest_rate), years) / 100
  @assertion.assert_eq(1500, simple_interest)?
  
  // 计算总金额
  let total_amount = azimuth.add(principal, simple_interest)
  @assertion.assert_eq(11500, total_amount)?
  
  // 计算月供（简化）
  let monthly_payment = total_amount / (azimuth.multiply(years, 12))
  @assertion.assert_eq(319, monthly_payment)?  // 整数除法
}

test "statistical_calculations" {
  // 测试统计计算
  let numbers = [10, 20, 30, 40, 50]  // 假设的数组
  
  // 计算平均值（简化为前5个数的和除以5）
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(10, 20), 30), 40), 50)
  let average = sum / 5
  @assertion.assert_eq(30, average)?
  
  // 计算范围（最大值 - 最小值）
  let range = azimuth.add(50, -10)
  @assertion.assert_eq(40, range)?
  
  // 计算中位数（奇数个数）
  let median = 30  // 第三个数
  @assertion.assert_eq(30, median)?
}

test "geometry_and_trigonometry" {
  // 测试几何和三角学计算
  let radius = 7
  
  // 计算直径
  let diameter = azimuth.multiply(radius, 2)
  @assertion.assert_eq(14, diameter)?
  
  // 计算周长（简化为π ≈ 22/7）
  let circumference = azimuth.multiply(diameter, 22) / 7
  @assertion.assert_eq(44, circumference)?
  
  // 计算面积（简化为π ≈ 22/7）
  let area = azimuth.multiply(azimuth.multiply(radius, radius), 22) / 7
  @assertion.assert_eq(154, area)?
  
  // 计算立方体体积
  let side = 5
  let volume = azimuth.multiply(azimuth.multiply(side, side), side)
  @assertion.assert_eq(125, volume)?
}

test "time_and_date_calculations" {
  // 测试时间和日期计算
  let days_in_month = 30
  let months = 6
  
  // 计算总天数
  let total_days = azimuth.multiply(days_in_month, months)
  @assertion.assert_eq(180, total_days)?
  
  // 计算周数
  let weeks = total_days / 7
  @assertion.assert_eq(25, weeks)?  // 整数除法
  
  // 计算剩余天数
  let remaining_days = total_days - azimuth.multiply(weeks, 7)
  @assertion.assert_eq(5, remaining_days)?
  
  // 计算小时数
  let hours = azimuth.multiply(total_days, 24)
  @assertion.assert_eq(4320, hours)?
}

test "algorithm_complexity_analysis" {
  // 测试算法复杂度分析
  let n = 100
  
  // O(1) 常数时间
  let constant_time = 1
  @assertion.assert_eq(1, constant_time)?
  
  // O(n) 线性时间
  let linear_time = n
  @assertion.assert_eq(100, linear_time)?
  
  // O(n²) 二次时间
  let quadratic_time = azimuth.multiply(n, n)
  @assertion.assert_eq(10000, quadratic_time)?
  
  // O(log n) 对数时间（简化）
  let log_time = 7  // log2(100) ≈ 6.64，向上取整为7
  @assertion.assert_eq(7, log_time)?
  
  // O(n log n) 线性对数时间
  let n_log_n_time = azimuth.multiply(n, log_time)
  @assertion.assert_eq(700, n_log_n_time)?
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let zero = 0
  let one = 1
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试零运算
  @assertion.assert_eq(0, azimuth.add(zero, zero))?
  @assertion.assert_eq(0, azimuth.multiply(zero, max_int))?
  @assertion.assert_eq(0, azimuth.multiply(zero, min_int))?
  
  // 测试单位运算
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, one))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, one))?
  @assertion.assert_eq(azimuth.add(max_int, one), azimuth.add(one, max_int))?
  
  // 测试自反运算
  @assertion.assert_eq(2, azimuth.add(1, 1))?
  @assertion.assert_eq(4, azimuth.multiply(2, 2))?
  @assertion.assert_eq(0, azimuth.add(5, -5))?
}