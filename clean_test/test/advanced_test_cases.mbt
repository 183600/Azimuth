// 高级测试用例 - 测试 clean_test 模块的各种功能
// 包含实际应用场景和边界情况测试

test "complex_financial_calculation" {
  // 复杂金融计算：贷款计算
  // 假设贷款金额为100000，年利率5%，贷款期限3年
  // 简化的总还款计算：本金 + 本金×年利率×年限
  let principal = 100000
  let annual_rate = 5
  let years = 3
  
  let total_interest = @clean_test.multiply(principal, @clean_test.multiply(annual_rate, years))
  let total_repayment = @clean_test.add(principal, total_interest)
  
  // 验证总还款额：100000 + 100000×5×3 = 2500000
  @clean_test.assert_eq(2500000, total_repayment)
}

test "physics_simulation" {
  // 物理模拟：自由落体运动（简化版）
  // 公式：距离 = 初始速度 × 时间 + 0.5 × 重力加速度 × 时间²
  // 由于只有整数运算，我们简化为：距离 = 初始速度 × 时间 + 重力加速度 × 时间² / 2
  
  let initial_velocity = 10  // 初始速度 10 m/s
  let time = 5  // 时间 5 秒
  let gravity = 10  // 重力加速度 10 m/s²（简化值）
  
  let distance_velocity = @clean_test.multiply(initial_velocity, time)
  let time_squared = @clean_test.multiply(time, time)
  let distance_gravity = @clean_test.multiply(gravity, time_squared) / 2
  let total_distance = @clean_test.add(distance_velocity, distance_gravity)
  
  // 验证总距离：10×5 + 10×25/2 = 50 + 125 = 175
  @clean_test.assert_eq(175, total_distance)
}

test "array_index_calculation" {
  // 数组索引计算：二维数组转一维数组的索引计算
  // 公式：一维索引 = 行号 × 列数 + 列号
  
  let row = 3
  let col = 4
  let num_cols = 10
  
  let linear_index = @clean_test.add(@clean_test.multiply(row, num_cols), col)
  
  // 验证线性索引：3×10 + 4 = 34
  @clean_test.assert_eq(34, linear_index)
}

test "date_calculation" {
  // 日期计算：计算两个日期之间的天数（简化版）
  // 假设每月30天，每年12个月
  
  let year1 = 2023
  let month1 = 6
  let day1 = 15
  
  let year2 = 2024
  let month2 = 8
  let day2 = 20
  
  // 计算每个日期的总天数
  let days1 = @clean_test.add(@clean_test.add(
    @clean_test.multiply(year1, 360),  // 年份×360天
    @clean_test.multiply(month1, 30)   // 月份×30天
  ), day1)
  
  let days2 = @clean_test.add(@clean_test.add(
    @clean_test.multiply(year2, 360),  // 年份×360天
    @clean_test.multiply(month2, 30)   // 月份×30天
  ), day2)
  
  let days_difference = @clean_test.add(days2, -days1)
  
  // 验证天数差：(2024×360 + 8×30 + 20) - (2023×360 + 6×30 + 15)
  // = (728640 + 240 + 20) - (728280 + 180 + 15) = 728900 - 728475 = 425
  @clean_test.assert_eq(425, days_difference)
}

test "string_template_processing" {
  // 字符串模板处理：生成格式化的用户信息
  
  let username = "moonbit_dev"
  let user_id = 12345
  let role = "developer"
  
  // 生成用户信息字符串
  let user_info = @clean_test.greet(username)
  
  // 验证用户信息格式
  @clean_test.assert_eq_string("Hello, moonbit_dev!", user_info)
  
  // 测试特殊字符处理
  let special_username = "user@domain.com"
  let special_user_info = @clean_test.greet(special_username)
  @clean_test.assert_eq_string("Hello, user@domain.com!", special_user_info)
}

test "error_boundary_conditions" {
  // 错误边界条件测试：测试函数在极端条件下的行为
  
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000
  
  // 加法测试
  let add_result = @clean_test.add(large_num1, large_num2)
  @clean_test.assert_true(add_result > large_num1)
  
  // 乘法测试
  let multiply_result = @clean_test.multiply(large_num1, 100)
  @clean_test.assert_true(multiply_result > large_num1)
  
  // 测试负数边界
  let negative_large = -1000000
  let negative_add = @clean_test.add(negative_large, large_num1)
  @clean_test.assert_eq(0, negative_add)
}

test "algorithm_complexity_test" {
  // 算法复杂度测试：模拟简单算法的时间复杂度计算
  
  // 模拟 O(n²) 算法的操作次数
  let n = 100
  let operations = @clean_test.multiply(n, n)
  
  // 验证操作次数：100² = 10000
  @clean_test.assert_eq(10000, operations)
  
  // 模拟 O(n log n) 算法的操作次数（简化版）
  // 假设 log₂100 ≈ 7（整数近似）
  let log_approx = 7
  let n_log_n_ops = @clean_test.multiply(n, log_approx)
  
  // 验证操作次数：100 × 7 = 700
  @clean_test.assert_eq(700, n_log_n_ops)
}

test "statistical_calculations" {
  // 统计计算：计算平均值和总和
  
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let num5 = 88
  
  // 计算总和
  let sum = @clean_test.add(@clean_test.add(@clean_test.add(@clean_test.add(num1, num2), num3), num4), num5)
  
  // 计算平均值（整数除法）
  let average = sum / 5
  
  // 验证总和：85 + 90 + 78 + 92 + 88 = 433
  @clean_test.assert_eq(433, sum)
  
  // 验证平均值：433 / 5 = 86（整数除法）
  @clean_test.assert_eq(86, average)
}

test "game_logic_calculation" {
  // 游戏逻辑计算：RPG游戏中的伤害计算
  
  let base_damage = 100
  let weapon_bonus = 25
  let skill_multiplier = 2
  let enemy_defense = 30
  
  // 计算最终伤害：(基础伤害 + 武器加成) × 技能倍率 - 敌人防御
  let total_damage = @clean_test.add(
    @clean_test.multiply(
      @clean_test.add(base_damage, weapon_bonus),
      skill_multiplier
    ),
    -enemy_defense
  )
  
  // 验证最终伤害：(100 + 25) × 2 - 30 = 125 × 2 - 30 = 250 - 30 = 220
  @clean_test.assert_eq(220, total_damage)
}
