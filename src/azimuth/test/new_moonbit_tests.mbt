// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和断言

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  // 小正数
  azimuth::assert_eq(7, azimuth::add(3, 4))
  
  // 小负数
  azimuth::assert_eq(-7, azimuth::add(-3, -4))
  
  // 混合符号
  azimuth::assert_eq(-1, azimuth::add(3, -4))
  azimuth::assert_eq(1, azimuth::add(-3, 4))
  
  // 大数运算
  azimuth::assert_eq(2000000, azimuth::add(1000000, 1000000))
  azimuth::assert_eq(-2000000, azimuth::add(-1000000, -1000000))
}

test "multiplication_edge_cases" {
  // 测试乘法的边缘情况
  // 零乘法
  azimuth::assert_eq(0, azimuth::multiply(0, 12345))
  azimuth::assert_eq(0, azimuth::multiply(-12345, 0))
  
  // 单位乘法
  azimuth::assert_eq(12345, azimuth::multiply(1, 12345))
  azimuth::assert_eq(-12345, azimuth::multiply(-1, 12345))
  
  // 大数乘法
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  azimuth::assert_eq(-1000000, azimuth::multiply(-1000, 1000))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  // 中文字符
  azimuth::assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  
  // 日文字符
  azimuth::assert_eq_string("Hello, 田中!", azimuth::greet("田中"))
  
  // 韩文字符
  azimuth::assert_eq_string("Hello, 김철수!", azimuth::greet("김철수"))
  
  // 俄文字符
  azimuth::assert_eq_string("Hello, Иван!", azimuth::greet("Иван"))
  
  // 阿拉伯文字符
  azimuth::assert_eq_string("Hello, محمد!", azimuth::greet("محمد"))
}

test "complex_financial_calculations" {
  // 测试复杂金融计算场景
  // 计算贷款利息：本金 × 利率 × 期限
  let principal = 100000  // 10万本金
  let rate = 5  // 5%年利率
  let years = 3  // 3年期限
  
  // 简化计算：总利息 = 本金 × 利率 × 年数
  let total_interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  azimuth::assert_eq(1500000, total_interest)
  
  // 计算还款总额：本金 + 利息
  let total_payment = azimuth::add(principal, total_interest)
  azimuth::assert_eq(1600000, total_payment)
}

test "physics_calculations" {
  // 测试物理计算场景
  // 计算动能：E = 1/2 × m × v²
  let mass = 10  // 10kg
  let velocity = 20  // 20m/s
  
  // 简化计算：E = m × v × v / 2
  let velocity_squared = azimuth::multiply(velocity, velocity)
  let energy = azimuth::multiply(mass, velocity_squared) / 2
  azimuth::assert_eq(2000, energy)
  
  // 计算动量：p = m × v
  let momentum = azimuth::multiply(mass, velocity)
  azimuth::assert_eq(200, momentum)
}

test "statistical_analysis" {
  // 测试统计分析计算
  // 计算一组数据的平均值
  let data_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(85, 90), 78), 92), 95)
  let data_count = 5
  let average = data_sum / data_count
  azimuth::assert_eq(88, average)
  
  // 计算方差（简化版）
  let diff1 = azimuth::add(85, -average)
  let diff2 = azimuth::add(90, -average)
  let diff3 = azimuth::add(78, -average)
  let diff4 = azimuth::add(92, -average)
  let diff5 = azimuth::add(95, -average)
  
  let variance = azimuth::add(azimuth::add(azimuth::add(azimuth::add(
    azimuth::multiply(diff1, diff1),
    azimuth::multiply(diff2, diff2)),
    azimuth::multiply(diff3, diff3)),
    azimuth::multiply(diff4, diff4)),
    azimuth::multiply(diff5, diff5)) / data_count
  
  azimuth::assert_eq(34, variance)
}

test "coordinate_geometry" {
  // 测试坐标几何计算
  // 计算两点间距离的平方：(x2-x1)² + (y2-y1)²
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  let dx = azimuth::add(x2, -x1)
  let dy = azimuth::add(y2, -y1)
  let distance_squared = azimuth::add(azimuth::multiply(dx, dx), azimuth::multiply(dy, dy))
  azimuth::assert_eq(25, distance_squared)
  
  // 计算三角形面积（使用海伦公式的简化版）
  // 边长：3, 4, 5 的直角三角形
  let a = 3
  let b = 4
  let c = 5
  let s = azimuth::add(azimuth::add(a, b), c) / 2  // 半周长
  // 简化面积计算：直角三角形面积 = 底 × 高 / 2
  let area = azimuth::multiply(a, b) / 2
  azimuth::assert_eq(6, area)
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  // 计算排列数 P(n,k) = n × (n-1) × ... × (n-k+1)
  // P(5,3) = 5 × 4 × 3 = 60
  let n = 5
  let k = 3
  let permutation = azimuth::multiply(n, azimuth::multiply(n - 1, n - 2))
  azimuth::assert_eq(60, permutation)
  
  // 计算组合数 C(n,k) = P(n,k) / k!
  // C(5,3) = 60 / (3 × 2 × 1) = 10
  let k_factorial = azimuth::multiply(k, azimuth::multiply(k - 1, k - 2))
  let combination = permutation / k_factorial
  azimuth::assert_eq(10, combination)
}

test "time_conversion_utilities" {
  // 测试时间转换工具
  // 将天数转换为小时、分钟、秒
  let days = 2
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  // 转换为小时
  let total_hours = azimuth::multiply(days, hours_per_day)
  azimuth::assert_eq(48, total_hours)
  
  // 转换为分钟
  let total_minutes = azimuth::multiply(total_hours, minutes_per_hour)
  azimuth::assert_eq(2880, total_minutes)
  
  // 转换为秒
  let total_seconds = azimuth::multiply(total_minutes, seconds_per_minute)
  azimuth::assert_eq(172800, total_seconds)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟 O(n²) 算法的时间复杂度
  let n = 10
  // 简化的复杂度计算：n × (n+1) / 2
  let operations = azimuth::multiply(n, azimuth::add(n, 1)) / 2
  azimuth::assert_eq(55, operations)
  
  // 模拟 O(n³) 算法的时间复杂度
  // 简化计算：n² × n
  let n_squared = azimuth::multiply(n, n)
  let cubic_operations = azimuth::multiply(n_squared, n)
  azimuth::assert_eq(1000, cubic_operations)
  
  // 模拟 O(log n) 算法的时间复杂度（简化为连续除以2）
  let log_value = n
  let log_operations = azimuth::add(log_value / 2, log_value / 4) + log_value / 8 + log_value / 16
  azimuth::assert_eq(9, log_operations)
}