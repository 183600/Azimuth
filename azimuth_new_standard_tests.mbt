// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "binary_number_operations" {
  // 二进制数操作测试
  let decimal_num = 10
  
  // 模拟二进制左移操作（乘以2）
  let left_shift = azimuth.multiply(decimal_num, 2)
  @assertion.assert_eq(20, left_shift)?
  
  // 模拟二进制右移操作（除以2）
  let right_shift = decimal_num / 2
  @assertion.assert_eq(5, right_shift)?
  
  // 模拟按位与操作（简化为取余）
  let bitwise_and = decimal_num % 4
  @assertion.assert_eq(2, bitwise_and)?
}

test "currency_exchange_calculation" {
  // 货币兑换计算测试
  let usd_amount = 1000
  let exchange_rate = 6.5  // 简化为整数65，表示6.5
  let fee_rate = 2  // 2%手续费
  
  // 计算兑换金额（使用整数表示小数）
  let exchanged_amount = azimuth.multiply(usd_amount, 65) / 10
  @assertion.assert_eq(6500, exchanged_amount)?
  
  // 计算手续费
  let fee = azimuth.multiply(exchanged_amount, fee_rate) / 100
  @assertion.assert_eq(130, fee)?
  
  // 计算最终到账金额
  let final_amount = azimuth.add(exchanged_amount, -fee)
  @assertion.assert_eq(6370, final_amount)?
}

test "physics_kinematics" {
  // 物理运动学计算测试
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 2      // 加速度 m/s²
  let time = 5              // 时间 s
  
  // 计算末速度：v = u + at
  let final_velocity = azimuth.add(initial_velocity, azimuth.multiply(acceleration, time))
  @assertion.assert_eq(20, final_velocity)?
  
  // 计算位移：s = ut + 0.5at²（简化为整数计算）
  let displacement = azimuth.add(
    azimuth.multiply(initial_velocity, time),
    azimuth.multiply(acceleration, azimuth.multiply(time, time)) / 2
  )
  @assertion.assert_eq(75, displacement)?
}

test "array_indexing_simulation" {
  // 数组索引模拟测试
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算二维数组在一维数组中的索引
  let linear_index = azimuth.add(azimuth.multiply(row_index, cols), col_index)
  @assertion.assert_eq(13, linear_index)?
  
  // 测试边界情况
  let first_index = azimuth.add(azimuth.multiply(0, cols), 0)
  @assertion.assert_eq(0, first_index)?
  
  let last_index = azimuth.add(azimuth.multiply(azimuth.add(rows, -1), cols), azimuth.add(cols, -1))
  @assertion.assert_eq(19, last_index)?
}

test "percentage_calculations" {
  // 百分比计算测试
  let total_value = 850
  let percentage_25 = 25
  let percentage_75 = 75
  
  // 计算25%
  let quarter_value = azimuth.multiply(total_value, percentage_25) / 100
  @assertion.assert_eq(212, quarter_value)?
  
  // 计算75%
  let three_quarter_value = azimuth.multiply(total_value, percentage_75) / 100
  @assertion.assert_eq(637, three_quarter_value)?
  
  // 计算增长后的值
  let growth_rate = 15
  let grown_value = azimuth.add(
    total_value,
    azimuth.multiply(total_value, growth_rate) / 100
  )
  @assertion.assert_eq(977, grown_value)?
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let username = "user123"
  let domain = "example.com"
  
  // 模拟字符串拼接生成邮箱
  let email = username + "@" + domain
  @assertion.assert_eq_string("user123@example.com", email)?
  
  // 测试greet函数处理特殊字符
  let special_chars = "!@#$%^&*()"
  let greet_result = azimuth.greet(special_chars)
  @assertion.assert_eq_string("Hello, !@#$%^&*()!", greet_result)?
  
  // 测试数字字符串
  let numeric_string = "20231234"
  let numeric_greet = azimuth.greet(numeric_string)
  @assertion.assert_eq_string("Hello, 20231234!", numeric_greet)?
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 100
  let level_multiplier = 3
  let accuracy_bonus = 50
  let time_penalty = 20
  let combo_multiplier = 2
  
  // 计算基础得分
  let level_score = azimuth.multiply(base_score, level_multiplier)
  
  // 计算准确率加成
  let accuracy_score = azimuth.add(level_score, accuracy_bonus)
  
  // 计算时间惩罚
  let time_score = azimuth.add(accuracy_score, -time_penalty)
  
  // 计算连击加成
  let final_score = azimuth.multiply(time_score, combo_multiplier)
  
  @assertion.assert_eq(460, final_score)?
  
  // 测试排名计算（前5%额外加分）
  let top_5_percent = 5
  let rank_bonus = azimuth.multiply(final_score, top_5_percent) / 100
  let rank_total = azimuth.add(final_score, rank_bonus)
  
  @assertion.assert_eq(483, rank_total)?
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let power_rating = 100  // 功率瓦数
  let hours_per_day = 8   // 每日使用小时数
  let days_per_month = 30 // 每月天数
  let electricity_rate = 12 // 电价分/千瓦时
  
  // 计算每日能耗（瓦时）
  let daily_consumption = azimuth.multiply(power_rating, hours_per_day)
  @assertion.assert_eq(800, daily_consumption)?
  
  // 计算月度能耗（瓦时）
  let monthly_consumption = azimuth.multiply(daily_consumption, days_per_month)
  @assertion.assert_eq(24000, monthly_consumption)?
  
  // 转换为千瓦时
  let monthly_kwh = monthly_consumption / 1000
  @assertion.assert_eq(24, monthly_kwh)?
  
  // 计算月度电费（分）
  let monthly_cost = azimuth.multiply(monthly_kwh, electricity_rate)
  @assertion.assert_eq(288, monthly_cost)?
}

test "mathematical_series" {
  // 数学级数计算测试
  // 计算等差数列前n项和：1 + 4 + 7 + ... + (3n-2)
  let n = 6
  let first_term = 1
  let common_diff = 3
  let last_term = azimuth.add(first_term, azimuth.multiply(common_diff, azimuth.add(n, -1)))
  
  // 使用求和公式：n × (首项 + 末项) / 2
  let arithmetic_sum = azimuth.multiply(n, azimuth.add(first_term, last_term)) / 2
  @assertion.assert_eq(63, arithmetic_sum)?
  
  // 计算等比数列前n项和：2 + 4 + 8 + 16 + ... + 2^n
  let geometric_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(2, 4), 8), 16), 32)
  @assertion.assert_eq(62, geometric_sum)?
}

test "data_validation_logic" {
  // 数据验证逻辑测试
  let input_value = 42
  let min_threshold = 10
  let max_threshold = 100
  let warning_threshold = 80
  
  // 验证输入值在有效范围内
  let is_valid = azimuth.add(input_value, -min_threshold) >= 0 && azimuth.add(max_threshold, -input_value) >= 0
  @assertion.assert_true(is_valid)?
  
  // 检查是否超过警告阈值
  let needs_warning = input_value >= warning_threshold
  @assertion.assert_false(needs_warning)?
  
  // 计算输入值在范围内的百分比位置
  let range_size = azimuth.add(max_threshold, -min_threshold)
  let position_in_range = azimuth.add(input_value, -min_threshold)
  let percentage_position = azimuth.multiply(position_in_range, 100) / range_size
  @assertion.assert_eq(35, percentage_position)?
}