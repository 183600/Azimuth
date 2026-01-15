// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_concatenation_with_unicode" {
  // 测试包含 Unicode 字符的字符串连接
  let result = @azimuth.greet("世界")
  @azimuth.assert_eq_string("Hello, 世界!", result)
}

test "mathematical_sequence_fibonacci" {
  // 计算斐波那契数列的前几项
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = @azimuth.add(1, 1)
  @azimuth.assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = @azimuth.add(f3, 1)
  @azimuth.assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = @azimuth.add(f4, f3)
  @azimuth.assert_eq(5, f5)
}

test "geometric_area_calculations" {
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 10
  let height = 4
  
  let sum_of_bases = @azimuth.add(top_base, bottom_base)  // 15
  let area = @azimuth.multiply(sum_of_bases, height) / 2  // 15 * 4 / 2 = 30
  @azimuth.assert_eq(30, area)
  
  // 计算平行四边形面积：底 × 高
  let base = 8
  let para_height = 6
  let para_area = @azimuth.multiply(base, para_height)
  @azimuth.assert_eq(48, para_area)
}

test "currency_calculation_with_discount" {
  // 带折扣的货币计算
  let original_price = 1000
  let discount_percentage = 15  // 15% 折扣
  let tax_percentage = 8       // 8% 税费
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(150, discount_amount)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(850, discounted_price)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  @azimuth.assert_eq(68, tax_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(918, final_price)
}

test "digital_root_calculation" {
  // 计算数字根：将一个数的各位数字相加，直到得到一位数
  // 对于 9875：9 + 8 + 7 + 5 = 29，然后 2 + 9 = 11，最后 1 + 1 = 2
  let number = 9875
  
  // 分解数字（简化方法，使用除法和取模）
  let digit1 = number / 1000        // 9
  let digit2 = (number / 100) % 10  // 8
  let digit3 = (number / 10) % 10   // 7
  let digit4 = number % 10          // 5
  
  // 第一次求和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(digit1, digit2), digit3), digit4)
  @azimuth.assert_eq(29, sum1)
  
  // 第二次求和
  let sum2_digit1 = sum1 / 10
  let sum2_digit2 = sum1 % 10
  let sum2 = @azimuth.add(sum2_digit1, sum2_digit2)
  @azimuth.assert_eq(11, sum2)
  
  // 第三次求和
  let sum3_digit1 = sum2 / 10
  let sum3_digit2 = sum2 % 10
  let sum3 = @azimuth.add(sum3_digit1, sum3_digit2)
  @azimuth.assert_eq(2, sum3)
}

test "time_zone_conversion" {
  // 时区转换测试
  let local_hour = 14  // 下午2点
  let time_difference = 8  // 与UTC时差8小时
  
  // 转换为UTC时间
  let utc_hour = local_hour - time_difference
  let normalized_utc_hour = if utc_hour < 0 { utc_hour + 24 } else { utc_hour }
  @azimuth.assert_eq(6, normalized_utc_hour)
  
  // 从UTC转换回本地时间
  let converted_local = normalized_utc_hour + time_difference
  let normalized_local = if converted_local >= 24 { converted_local - 24 } else { converted_local }
  @azimuth.assert_eq(14, normalized_local)
}

test "compound_interest_with_periods" {
  // 复利计算：本金 × (1 + 利率/期数)^期数
  // 简化计算，避免浮点数
  let principal = 10000
  let annual_rate = 12  // 12% 年利率
  let periods_per_year = 4  // 每年4期（季度）
  let years = 2
  
  // 每期利率：12% / 4 = 3%
  let period_rate = annual_rate / periods_per_year
  @azimuth.assert_eq(3, period_rate)
  
  // 总期数：4期/年 × 2年 = 8期
  let total_periods = @azimuth.multiply(periods_per_year, years)
  @azimuth.assert_eq(8, total_periods)
  
  // 简化计算：使用线性近似而非指数增长
  // 本金 × (1 + 期数 × 每期利率)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(total_periods, period_rate))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(124000, final_amount)
}

test "array_manipulation_simulation" {
  // 模拟数组操作：计算数组元素的总和与平均值
  // 假设数组元素为：10, 20, 30, 40, 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算加权平均值（权重分别为1, 2, 3, 4, 5）
  let weighted_sum = @azimuth.add(
    @azimuth.multiply(10, 1),
    @azimuth.add(
      @azimuth.multiply(20, 2),
      @azimuth.add(
        @azimuth.multiply(30, 3),
        @azimuth.add(
          @azimuth.multiply(40, 4),
          @azimuth.multiply(50, 5)
        )
      )
    )
  )
  @azimuth.assert_eq(550, weighted_sum)
  
  // 权重总和
  let weight_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, weight_sum)
  
  // 加权平均值
  let weighted_average = weighted_sum / weight_sum
  @azimuth.assert_eq(36, weighted_average)
}

test "game_logic_simulation" {
  // 游戏逻辑模拟：计算经验值和等级
  let base_exp = 100
  let level = 5
  let exp_multiplier = 150
  
  // 计算升级所需经验值：基础经验 × 等级 × 经验倍率
  let required_exp = @azimuth.multiply(@azimuth.multiply(base_exp, level), exp_multiplier) / 100
  @azimuth.assert_eq(750, required_exp)
  
  // 计算总经验值：前几级经验值之和
  // 简化计算：使用等差数列求和公式
  let total_exp = @azimuth.multiply(level, @azimuth.add(level, 1)) * base_exp / 2
  @azimuth.assert_eq(1500, total_exp)
  
  // 计算完成度百分比
  let current_exp = 1200
  let completion_percentage = @azimuth.multiply(current_exp, 100) / total_exp
  @azimuth.assert_eq(80, completion_percentage)
}

test "physics_simulation_simple" {
  // 简单物理模拟：计算自由落体运动
  let initial_velocity = 0  // 初始速度为0
  let gravity = 9.8         // 重力加速度（简化为10）
  let time = 5              // 时间（秒）
  
  // 使用简化重力值
  let simplified_gravity = 10
  
  // 计算最终速度：v = v0 + g × t
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(simplified_gravity, time))
  @azimuth.assert_eq(50, final_velocity)
  
  // 计算下落距离：s = v0 × t + 0.5 × g × t²
  // 简化计算：s = g × t² / 2
  let distance = @azimuth.multiply(simplified_gravity, @azimuth.multiply(time, time)) / 2
  @azimuth.assert_eq(125, distance)
  
  // 计算平均速度：v_avg = (v0 + v) / 2
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  @azimuth.assert_eq(25, average_velocity)
  
  // 验证距离也可以通过平均速度计算：s = v_avg × t
  let distance_check = @azimuth.multiply(average_velocity, time)
  @azimuth.assert_eq(distance, distance_check)
}