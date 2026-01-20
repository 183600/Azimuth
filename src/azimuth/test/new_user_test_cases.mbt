// 新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试项目的核心功能

// 导入必要的包
test "arithmetic_sequence_sum" {

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(35, sum)
}

test "geometric_progression" {
  // 测试等比数列：2, 6, 18, 54
  // 计算前4项的和
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 3)  // 6
  let term3 = @azimuth.multiply(term2, 3)  // 18
  let term4 = @azimuth.multiply(term3, 3)  // 54
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, sum)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percent = 15  // 15% 折扣
  let tax_percent = 8        // 8% 税费
  
  // 计算折后价格
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算含税价格
  let tax_amount = @azimuth.multiply(discounted_price, tax_percent) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(918, final_price)  // 1000 - 150 + 68 = 918
}

test "speed_conversion" {
  // 测试速度单位转换：km/h 转 m/s
  // 简化公式：m/s = km/h × 5/18 ≈ km/h / 3.6 ≈ km/h × 10 / 36
  let kmh_speed = 72
  let ms_speed = @azimuth.multiply(kmh_speed, 10) / 36
  @azimuth.assert_eq(20, ms_speed)
  
  // 反向转换：m/s 转 km/h
  let ms_speed_back = 20
  let kmh_speed_back = @azimuth.multiply(ms_speed_back, 36) / 10
  @azimuth.assert_eq(72, kmh_speed_back)
}

test "compound_interest_simplified" {
  // 测试简化的复利计算
  let principal = 5000
  let rate_percent = 6
  let years = 3
  
  // 简化计算：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(95000, total)  // 5000 × (1 + 6×3) = 5000 × 19 = 95000
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  // 测试包含各种字符的字符串
  let test_cases = [
    ("simple", "Hello, simple!"),
    ("With Spaces", "Hello, With Spaces!"),
    ("123numbers", "Hello, 123numbers!"),
    ("special@#$%", "Hello, special@#$%!"),
    ("中文测试", "Hello, 中文测试!"),
    ("🌟emoji", "Hello, 🌟emoji!")
  ]
  
  // 测试第一个和最后一个案例
  @azimuth.assert_eq_string("Hello, simple!", @azimuth.greet("simple"))
  @azimuth.assert_eq_string("Hello, 🌟emoji!", @azimuth.greet("🌟emoji"))
}

test "matrix_operations_simulation" {
  // 模拟简单的矩阵运算
  // 2x2 矩阵乘法：[[a, b], [c, d]] × [[e, f], [g, h]]
  let a = 1; let b = 2; let c = 3; let d = 4
  let e = 5; let f = 6; let g = 7; let h = 8
  
  // 计算结果矩阵的元素
  // 结果[0,0] = a*e + b*g
  let result_00 = @azimuth.add(@azimuth.multiply(a, e), @azimuth.multiply(b, g))
  // 结果[0,1] = a*f + b*h
  let result_01 = @azimuth.add(@azimuth.multiply(a, f), @azimuth.multiply(b, h))
  // 结果[1,0] = c*e + d*g
  let result_10 = @azimuth.add(@azimuth.multiply(c, e), @azimuth.multiply(d, g))
  // 结果[1,1] = c*f + d*h
  let result_11 = @azimuth.add(@azimuth.multiply(c, f), @azimuth.multiply(d, h))
  
  @azimuth.assert_eq(19, result_00)  // 1*5 + 2*7 = 5 + 14 = 19
  @azimuth.assert_eq(22, result_01)  // 1*6 + 2*8 = 6 + 16 = 22
  @azimuth.assert_eq(43, result_10)  // 3*5 + 4*7 = 15 + 28 = 43
  @azimuth.assert_eq(50, result_11)  // 3*6 + 4*8 = 18 + 32 = 50
}

test "game_logic_simulation" {
  // 测试游戏逻辑模拟
  let player_level = 5
  let base_damage = 100
  let weapon_multiplier = 150  // 1.5倍，用150表示
  let critical_chance = 20     // 20%暴击率
  
  // 计算基础伤害
  let weapon_damage = @azimuth.multiply(base_damage, weapon_multiplier) / 100
  let level_bonus = @azimuth.multiply(player_level, 10)
  let total_damage = @azimuth.add(weapon_damage, level_bonus)
  
  // 模拟暴击（简化：总是触发暴击）
  let critical_damage = @azimuth.multiply(total_damage, 2)
  
  @azimuth.assert_eq(320, total_damage)     // 100*1.5 + 5*10 = 150 + 50 = 200
  @azimuth.assert_eq(640, critical_damage)  // 200 * 2 = 400
}

test "data_analysis_basic" {
  // 测试基础数据分析
  // 计算数据集：[12, 15, 18, 21, 24] 的统计量
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(12, 15), 18), 21), 24)
  let data_count = 5
  let mean = data_sum / data_count
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(12, -mean)
  let diff2 = @azimuth.add(15, -mean)
  let diff3 = @azimuth.add(18, -mean)
  let diff4 = @azimuth.add(21, -mean)
  let diff5 = @azimuth.add(24, -mean)
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)
  let squared_diff2 = @azimuth.multiply(diff2, diff2)
  let squared_diff3 = @azimuth.multiply(diff3, diff3)
  let squared_diff4 = @azimuth.multiply(diff4, diff4)
  let squared_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5)
  let variance = variance_sum / data_count
  
  @azimuth.assert_eq(90, data_sum)   // 12 + 15 + 18 + 21 + 24 = 90
  @azimuth.assert_eq(18, mean)      // 90 / 5 = 18
}

test "physics_simulation_simple" {
  // 测试简单物理模拟
  // 自由落体运动：h = v0*t + 0.5*g*t^2
  // 简化计算，忽略小数部分
  let initial_velocity = 10  // m/s
  let gravity = 10           // m/s^2 (简化值)
  let time = 3               // seconds
  
  // 计算位移
  let velocity_component = @azimuth.multiply(initial_velocity, time)
  let gravity_component = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2
  let displacement = @azimuth.add(velocity_component, gravity_component)
  
  // 计算末速度：v = v0 + g*t
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(gravity, time))
  
  @azimuth.assert_eq(75, displacement)   // 10*3 + 10*9/2 = 30 + 45 = 75
  @azimuth.assert_eq(40, final_velocity)  // 10 + 10*3 = 40
}