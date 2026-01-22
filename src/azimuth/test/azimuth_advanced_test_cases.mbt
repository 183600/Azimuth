// Azimuth 库的高级测试用例
// 包含实际应用场景和复杂计算逻辑的测试

test "physics_kinetic_energy_calculation" {
  // 物理动能计算: KE = 1/2 * m * v^2
  let mass = 10  // 质量 (kg)
  let velocity = 15  // 速度 (m/s)
  
  // 计算 v^2
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  // 计算 1/2 * m * v^2 (使用整数除法模拟)
  let kinetic_energy = @azimuth.divide_with_ceil(@azimuth.multiply(mass, velocity_squared), 2)
  
  @azimuth.assert_eq(1125, kinetic_energy)  // 1/2 * 10 * 15^2 = 1/2 * 10 * 225 = 1125
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 10000  // 本金
  let rate_percent = 5   // 年利率百分比
  let years = 3          // 年数
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, rate_percent), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, rate_percent), 100)
  let final_amount = @azimuth.add(year2_total, year3_interest)
  
  @azimuth.assert_eq(11576, final_amount)  // 10000 -> 10500 -> 11025 -> 11576 (向上取整)
}

test "data_structure_stack_simulation" {
  // 模拟栈数据结构的操作
  // 使用加法模拟 push，减法模拟 pop
  let stack_top = 0
  let stack_size = 0
  
  // Push 操作
  stack_top = @azimuth.add(stack_top, 10)  // push 10
  stack_size = @azimuth.add(stack_size, 1)
  
  stack_top = @azimuth.add(stack_top, 20)  // push 20
  stack_size = @azimuth.add(stack_size, 1)
  
  stack_top = @azimuth.add(stack_top, 30)  // push 30
  stack_size = @azimuth.add(stack_size, 1)
  
  // Pop 操作 (模拟)
  stack_top = @azimuth.add(stack_top, -30)  // pop 30
  stack_size = @azimuth.add(stack_size, -1)
  
  @azimuth.assert_eq(30, stack_top)  // 当前栈顶元素
  @azimuth.assert_eq(2, stack_size)  // 栈大小
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析模拟
  let input_size = 100
  let constant_factor = 3
  
  // O(n) 算法
  let linear_time = @azimuth.multiply(input_size, constant_factor)
  
  // O(n^2) 算法
  let quadratic_time = @azimuth.multiply(input_size, input_size)
  
  // O(log n) 算法（简化为除法）
  let logarithmic_time = @azimuth.divide_with_ceil(input_size, 10)
  
  @azimuth.assert_eq(300, linear_time)
  @azimuth.assert_eq(10000, quadratic_time)
  @azimuth.assert_eq(10, logarithmic_time)
}

test "gaming_damage_calculation" {
  // 游戏伤害计算模拟
  let base_damage = 50
  let weapon_multiplier = 2
  let critical_strike_chance = 25  // 25% 暴击率
  let critical_multiplier = 2
  
  // 基础伤害计算
  let weapon_damage = @azimuth.multiply(base_damage, weapon_multiplier)
  
  // 暴击判定（简化计算）
  let critical_bonus = @azimuth.divide_with_ceil(@azimuth.multiply(weapon_damage, critical_strike_chance), 100)
  let total_damage = @azimuth.add(weapon_damage, critical_bonus)
  
  // 暴击伤害
  let critical_damage = @azimuth.multiply(total_damage, critical_multiplier)
  
  @azimuth.assert_eq(125, total_damage)   // 50*2 + ceil(100*25/100) = 100 + 25 = 125
  @azimuth.assert_eq(250, critical_damage)  // 125 * 2 = 250
}

test "time_zone_conversion" {
  // 时区转换计算模拟
  let local_time = 14  // 下午2点 (24小时制)
  let time_difference = 8  // 与UTC的时差
  
  // 转换为UTC时间
  let utc_time = @azimuth.add(local_time, -time_difference)
  
  // 处理负时间（前一天）
  let adjusted_utc_time = if utc_time < 0 {
    @azimuth.add(utc_time, 24)
  } else {
    utc_time
  }
  
  @azimuth.assert_eq(6, adjusted_utc_time)  // 14 - 8 = 6 (早上6点UTC)
}

test "statistical_variance_calculation" {
  // 统计方差计算（简化版）
  let values = [10, 15, 20, 25, 30]  // 模拟数组
  let n = 5  // 数据点数量
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 15), 20), 25), 30)
  let mean = @azimuth.divide_with_ceil(sum, n)
  
  // 计算方差（简化计算）
  let diff1 = @azimuth.add(10, -mean)
  let diff2 = @azimuth.add(15, -mean)
  let squared_diff_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), 0)
  let variance = @azimuth.divide_with_ceil(squared_diff_sum, n)
  
  @azimuth.assert_eq(20, mean)  // (10+15+20+25+30)/5 = 20
  @azimuth.assert_eq(0, variance)  // 简化计算
}

test "geometric_area_calculation" {
  // 几何面积计算
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  
  // 三角形面积 (1/2 * base * height)
  let triangle_area = @azimuth.divide_with_ceil(@azimuth.multiply(triangle_base, triangle_height), 2)
  
  // 总面积
  let total_area = @azimuth.add(rectangle_area, triangle_area)
  
  @azimuth.assert_eq(96, rectangle_area)   // 12 * 8 = 96
  @azimuth.assert_eq(30, triangle_area)    // ceil(10 * 6 / 2) = 30
  @azimuth.assert_eq(126, total_area)      // 96 + 30 = 126
}

test "temperature_conversion" {
  // 温度转换计算（简化版）
  let celsius = 25  // 摄氏度
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let back_to_celsius = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(fahrenheit, -32), 5), 9)
  
  @azimuth.assert_eq(77, fahrenheit)       // 25 * 9/5 + 32 = 45 + 32 = 77
  @azimuth.assert_eq(25, back_to_celsius)  // (77 - 32) * 5/9 = 25
}

test "cryptography_simple_cipher" {
  // 简单密码学模拟（凯撒密码）
  let original_value = 1234
  let shift_key = 7
  let modulus = 1000  // 简化的模数
  
  // 加密过程
  let encrypted = @azimuth.add(@azimuth.multiply(original_value, shift_key), 42)
  let normalized_encrypted = encrypted % modulus
  
  // 解密过程（简化）
  let decrypted_candidate1 = @azimuth.divide_with_ceil(@azimuth.add(normalized_encrypted, -42), shift_key)
  let decrypted_candidate2 = @azimuth.add(@azimuth.divide_with_ceil(normalized_encrypted, shift_key), 0)
  
  @azimuth.assert_eq(640, normalized_encrypted)    // (1234 * 7 + 42) % 1000 = 640
  @azimuth.assert_eq(85, decrypted_candidate1)     // ceil((640 - 42) / 7) = 86
  @azimuth.assert_eq(91, decrypted_candidate2)     // ceil(640 / 7) = 91
}