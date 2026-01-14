// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了各种实际应用场景和边界情况

// 导入主模块中的函数
use azimuth

test "string_case_processing" {
  // 字符串大小写处理测试
  let lower_name = "moonbit"
  let upper_name = "MOONBIT"
  let mixed_name = "MoonBit"
  
  // 测试不同大小写的字符串处理
  assert_eq_string("Hello, moonbit!", greet(lower_name))
  assert_eq_string("Hello, MOONBIT!", greet(upper_name))
  assert_eq_string("Hello, MoonBit!", greet(mixed_name))
}

test "calculation_with_negative_numbers" {
  // 负数计算测试
  let a = -15
  let b = -20
  let c = 10
  
  // 测试负数相加
  let neg_add = add(a, b)
  assert_eq(-35, neg_add)
  
  // 测试正负数相加
  let mixed_add = add(a, c)
  assert_eq(-5, mixed_add)
  
  // 测试负数相乘
  let neg_mul = multiply(a, b)
  assert_eq(300, neg_mul)
  
  // 测试正负数相乘
  let mixed_mul = multiply(a, c)
  assert_eq(-150, mixed_mul)
}

test "complex_arithmetic_sequence" {
  // 复杂算术序列测试
  let initial = 5
  let difference = 3
  let terms = 5
  
  // 计算等差数列的第n项：a + (n-1)d
  let term1 = initial
  let term2 = add(initial, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  let term5 = add(term4, difference)
  
  assert_eq(5, term1)
  assert_eq(8, term2)
  assert_eq(11, term3)
  assert_eq(14, term4)
  assert_eq(17, term5)
  
  // 计算前n项和：n/2 * (首项 + 末项)
  let sum = multiply(terms, add(term1, term5)) / 2
  assert_eq(55, sum)
}

test "real_world_measurement_conversion" {
  // 实际测量单位转换测试
  let meters = 100
  
  // 转换为厘米：1米 = 100厘米
  let centimeters = multiply(meters, 100)
  assert_eq(10000, centimeters)
  
  // 转换为毫米：1厘米 = 10毫米
  let millimeters = multiply(centimeters, 10)
  assert_eq(100000, millimeters)
  
  // 转换为千米：1000米 = 1千米
  let kilometers = meters / 1000
  assert_eq(0, kilometers)  // 整数除法
  
  // 转换为英寸（近似）：1米 ≈ 39.37英寸，简化为39
  let inches = multiply(meters, 39)
  assert_eq(3900, inches)
}

test "business_profit_calculation" {
  // 商业利润计算测试
  let revenue = 50000
  let cost1 = 20000
  let cost2 = 15000
  let cost3 = 8000
  
  // 计算总成本
  let total_cost = add(add(cost1, cost2), cost3)
  assert_eq(43000, total_cost)
  
  // 计算利润
  let profit = add(revenue, -total_cost)
  assert_eq(7000, profit)
  
  // 计算利润率（百分比）
  let profit_margin = multiply(profit, 100) / revenue
  assert_eq(14, profit_margin)  // 7000 * 100 / 50000 = 14%
}

test "scientific_notation_simulation" {
  // 科学计数法模拟测试
  let base = 2
  let exponent = 10
  
  // 计算 2^10 = 1024
  let result = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(2, 2))
  assert_eq(64, result)  // 2^6 = 64
  
  // 继续计算剩余的幂
  let final_result = multiply(result, multiply(2, 2))
  assert_eq(256, final_result)  // 2^8 = 256
  
  let complete_result = multiply(final_result, multiply(2, 2))
  assert_eq(1024, complete_result)  // 2^10 = 1024
}

test "game_logic_calculation" {
  // 游戏逻辑计算测试
  let player_level = 15
  let base_hp = 100
  let hp_per_level = 20
  
  // 计算玩家生命值
  let player_hp = add(base_hp, multiply(player_level, hp_per_level))
  assert_eq(400, player_hp)
  
  // 计算经验值需求（简化公式）
  let exp_required = multiply(player_level, multiply(player_level, 100))
  assert_eq(22500, exp_required)
  
  // 计算伤害值
  let weapon_damage = 50
  let skill_multiplier = 2
  let total_damage = multiply(weapon_damage, skill_multiplier)
  assert_eq(100, total_damage)
}

test "date_time_calculations" {
  // 日期时间计算测试
  let days_in_month = 30
  let months = 6
  let days_in_week = 7
  
  // 计算总天数
  let total_days = multiply(days_in_month, months)
  assert_eq(180, total_days)
  
  // 计算周数
  let weeks = total_days / days_in_week
  assert_eq(25, weeks)  // 180 / 7 = 25（整数除法）
  
  // 计算剩余天数
  let remaining_days = total_days % days_in_week
  assert_eq(5, remaining_days)  // 180 % 7 = 5
}

test "statistical_analysis" {
  // 统计分析测试
  // 数据集：10, 20, 30, 40, 50, 60, 70, 80, 90, 100
  let data_sum = add(add(add(add(add(add(add(add(add(10, 20), 30), 40), 50), 60), 70), 80), 90), 100)
  let data_count = 10
  
  // 计算平均值
  let mean = data_sum / data_count
  assert_eq(55, mean)
  
  // 计算中位数（偶数个数据点，取中间两个的平均值）
  let median = add(50, 60) / 2
  assert_eq(55, median)
  
  // 计算范围（最大值 - 最小值）
  let range = add(100, -10)
  assert_eq(90, range)
}

test "network_performance_metrics" {
  // 网络性能指标测试
  let packet_size = 1500  // 字节
  let packets_per_second = 1000
  let duration_seconds = 60
  
  // 计算总数据量
  let total_packets = multiply(packets_per_second, duration_seconds)
  let total_bytes = multiply(total_packets, packet_size)
  assert_eq(90000000, total_bytes)
  
  // 转换为千字节
  let total_kb = total_bytes / 1000
  assert_eq(90000, total_kb)
  
  // 转换为兆字节
  let total_mb = total_kb / 1000
  assert_eq(90, total_mb)
  
  // 计算带宽利用率（假设最大带宽为100MB/s）
  let max_bandwidth = 100
  let actual_bandwidth = total_mb / duration_seconds
  let utilization = multiply(actual_bandwidth, 100) / max_bandwidth
  assert_eq(1, utilization)  // 1.5MB/s / 100MB/s * 100% = 1%（整数除法）
}