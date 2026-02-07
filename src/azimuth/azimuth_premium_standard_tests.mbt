// Azimuth 项目高级标准测试用例
// 包含10个专业级 MoonBit 测试用例，覆盖核心功能和边界情况

test "advanced_arithmetic_sequence" {
  // 等差数列计算测试
  let first_term = 3
  let difference = 4
  let n = 5
  
  // 计算第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(difference, subtract(n, 1)))
  
  // 计算前n项和：S_n = n/2 * (2a_1 + (n-1)d)
  let sum_step1 = multiply(2, first_term)
  let sum_step2 = add(sum_step1, multiply(difference, subtract(n, 1)))
  let sum_result = divide_with_ceil(multiply(n, sum_step2), 2)
  
  assert_eq(nth_term, 19)
  assert_eq(sum_result, 55)
}

test "geometric_progression_calculation" {
  // 等比数列计算测试
  let first_term = 2
  let ratio = 3
  let n = 4
  
  // 计算第n项：a_n = a_1 * r^(n-1)
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  // 计算前n项和：S_n = a_1 * (r^n - 1) / (r - 1)
  let ratio_power = multiply(multiply(ratio, ratio), multiply(ratio, ratio))
  let sum_numerator = subtract(ratio_power, 1)
  let sum_result = multiply(first_term, divide_with_ceil(sum_numerator, subtract(ratio, 1)))
  
  assert_eq(term2, 6)
  assert_eq(term3, 18)
  assert_eq(term4, 54)
  assert_eq(sum_result, 80)
}

test "temperature_conversion_comprehensive" {
  // 温度转换综合测试
  let celsius = 30
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let celsius_back_step1 = subtract(fahrenheit, 32)
  let celsius_back_step2 = multiply(celsius_back_step1, 5)
  let celsius_back = divide_with_ceil(celsius_back_step2, 9)
  
  // 开尔文温度：K = C + 273.15（简化为整数）
  let kelvin = add(celsius, 273)
  
  assert_eq(fahrenheit, 86)
  assert_eq(celsius_back, 30)
  assert_eq(kelvin, 303)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate = 8
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  // 总收益
  let total_profit = subtract(year3_total, principal)
  
  assert_eq(year1_total, 1080)
  assert_eq(year2_total, 1166)
  assert_eq(year3_total, 1259)
  assert_eq(total_profit, 259)
}

test "game_score_system" {
  // 游戏计分系统测试
  let base_score = 50
  let level = 4
  let combo_multiplier = 2
  let bonus = 30
  let penalty = 10
  
  // 基础分数 × 等级
  let level_score = multiply(base_score, level)
  
  // 加入连击倍数
  let combo_score = multiply(level_score, combo_multiplier)
  
  // 加入奖励
  let score_with_bonus = add(combo_score, bonus)
  
  // 减去惩罚
  let final_score = subtract(score_with_bonus, penalty)
  
  // 计算星级（每100分一星）
  let star_rating = divide_with_ceil(final_score, 100)
  
  assert_eq(level_score, 200)
  assert_eq(combo_score, 400)
  assert_eq(score_with_bonus, 430)
  assert_eq(final_score, 420)
  assert_eq(star_rating, 5)
}

test "inventory_optimization" {
  // 库存优化算法测试
  let initial_stock = 1000
  let monthly_demand = 150
  let lead_time = 2  // 交货周期（月）
  let safety_factor = 20  // 安全因子百分比
  
  // 计算再订货点
  let demand_during_lead = multiply(monthly_demand, lead_time)
  let safety_stock = divide_with_ceil(multiply(demand_during_lead, safety_factor), 100)
  let reorder_point = add(demand_during_lead, safety_stock)
  
  // 计算经济订货量（简化版）
  let holding_cost = 2
  let ordering_cost = 50
  let eoq_numerator = multiply(2, multiply(monthly_demand, ordering_cost))
  let eoq = divide_with_ceil(eoq_numerator, holding_cost)
  
  // 计算年订货次数
  let annual_orders = divide_with_ceil(multiply(monthly_demand, 12), eoq)
  
  assert_eq(demand_during_lead, 300)
  assert_eq(safety_stock, 60)
  assert_eq(reorder_point, 360)
  assert_eq(eoq, 7500)
  assert_eq(annual_orders, 3)
}

test "data_compression_analysis" {
  // 数据压缩分析测试
  let original_size = 2000
  let compression_ratio = 4
  let overhead = 50
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(original_size, compression_ratio)
  
  // 加入开销
  let final_compressed = add(compressed_size, overhead)
  
  // 计算实际压缩比
  let actual_ratio = divide_with_ceil(multiply(original_size, 100), final_compressed)
  
  // 计算节省空间
  let space_saved = subtract(original_size, final_compressed)
  let savings_percentage = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  // 计算压缩时间（假设每100字节需要1单位时间）
  let compression_time = divide_with_ceil(original_size, 100)
  
  assert_eq(compressed_size, 500)
  assert_eq(final_compressed, 550)
  assert_eq(actual_ratio, 363)
  assert_eq(space_saved, 1450)
  assert_eq(savings_percentage, 73)
  assert_eq(compression_time, 20)
}

test "network_performance_metrics" {
  // 网络性能指标测试
  let data_size = 10000  // 字节
  let bandwidth = 1000   // 字节/秒
  let latency = 50       // 毫秒
  let packet_loss = 5    // 百分比
  
  // 计算传输时间
  let transmission_time = divide_with_ceil(data_size, bandwidth)
  
  // 计算总延迟（传输时间 + 往返延迟）
  let total_delay = add(transmission_time, divide_with_ceil(latency, 1000))
  
  // 考虑丢包重传
  let effective_data = multiply(data_size, subtract(100, packet_loss))
  let actual_throughput = divide_with_ceil(effective_data, 100)
  
  // 计算效率
  let efficiency = divide_with_ceil(multiply(actual_throughput, 100), data_size)
  
  assert_eq(transmission_time, 10)
  assert_eq(total_delay, 11)
  assert_eq(actual_throughput, 9500)
  assert_eq(efficiency, 95)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 测试不同长度的名字
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  let greeting3 = greet(name3)
  
  // 测试包含数字和特殊字符的名字
  let greeting4 = greet("User123")
  let greeting5 = greet("test@example.com")
  
  // 测试空字符串和单字符
  let greeting6 = greet("")
  let greeting7 = greet("A")
  
  assert_eq_string(greeting1, "Hello, Alice!")
  assert_eq_string(greeting2, "Hello, Bob!")
  assert_eq_string(greeting3, "Hello, Charlie!")
  assert_eq_string(greeting4, "Hello, User123!")
  assert_eq_string(greeting5, "Hello, test@example.com!")
  assert_eq_string(greeting6, "Hello, !")
  assert_eq_string(greeting7, "Hello, A!")
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 计算平方和：1² + 2² + 3² + ... + n²
  let n = 5
  
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  let square5 = multiply(5, 5)
  
  let sum_of_squares = add(add(add(add(square1, square2), square3), square4), square5)
  
  // 计算立方和：1³ + 2³ + 3³ + ... + n³
  let cube1 = multiply(square1, 1)
  let cube2 = multiply(square2, 2)
  let cube3 = multiply(square3, 3)
  let cube4 = multiply(square4, 4)
  let cube5 = multiply(square5, 5)
  
  let sum_of_cubes = add(add(add(add(cube1, cube2), cube3), cube4), cube5)
  
  // 验证数学性质：平方和 = n(n+1)(2n+1)/6
  let formula_numerator = multiply(n, add(n, 1))
  let formula_with_2n_plus_1 = multiply(formula_numerator, add(multiply(2, n), 1))
  let square_sum_formula = divide_with_ceil(formula_with_2n_plus_1, 6)
  
  assert_eq(square1, 1)
  assert_eq(square2, 4)
  assert_eq(square3, 9)
  assert_eq(square4, 16)
  assert_eq(square5, 25)
  assert_eq(sum_of_squares, 55)
  assert_eq(cube1, 1)
  assert_eq(cube2, 8)
  assert_eq(cube3, 27)
  assert_eq(cube4, 64)
  assert_eq(cube5, 125)
  assert_eq(sum_of_cubes, 225)
  assert_eq(square_sum_formula, 55)
}