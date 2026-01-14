// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + ... + 20（前10个偶数）
  let n = 10
  let first = 2
  let last = 20
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(110, sum)  // 10 × (2 + 20) / 2 = 110
}

test "string_case_handling" {
  // 测试不同大小写的字符串处理
  let lowercase = "moonbit"
  let uppercase = "MOONBIT"
  let mixedcase = "MoonBit"
  
  @azimuth.assert_eq_string("Hello, moonbit!", @azimuth.greet(lowercase))
  @azimuth.assert_eq_string("Hello, MOONBIT!", @azimuth.greet(uppercase))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(mixedcase))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 15
  
  // 平方差恒等式：x² - 1 = (x - 1)(x + 1)
  let left_side = @azimuth.add(@azimuth.multiply(x, x), -1)
  let right_side = @azimuth.multiply(@azimuth.add(x, -1), @azimuth.add(x, 1))
  @azimuth.assert_eq(left_side, right_side)
  
  // 立方和恒等式：x³ + 1 = (x + 1)(x² - x + 1)
  let cube_left = @azimuth.add(@azimuth.multiply(@azimuth.multiply(x, x), x), 1)
  let cube_right = @azimuth.multiply(
    @azimuth.add(x, 1),
    @azimuth.add(@azimuth.add(@azimuth.multiply(x, x), @azimuth.multiply(x, -1)), 1)
  )
  @azimuth.assert_eq(cube_left, cube_right)
}

test "business_calculations" {
  // 业务计算测试：利润计算
  let revenue = 5000
  let cost = 3000
  let tax_rate = 20  // 20%
  
  // 计算利润
  let profit = @azimuth.add(revenue, -cost)
  
  // 计算税后利润
  let tax = @azimuth.multiply(profit, tax_rate) / 100
  let net_profit = @azimuth.add(profit, -tax)
  
  @azimuth.assert_eq(2000, profit)     // 5000 - 3000 = 2000
  @azimuth.assert_eq(400, tax)         // 2000 × 20% = 400
  @azimuth.assert_eq(1600, net_profit) // 2000 - 400 = 1600
}

test "physical_calculations" {
  // 物理计算测试：自由落体运动（简化版）
  let gravity = 10  // 重力加速度 m/s²（简化值）
  let time = 5      // 时间 s
  
  // 计算下落距离：s = 0.5 × g × t²
  // 由于是整数运算，我们简化为：s = g × t² / 2
  let time_squared = @azimuth.multiply(time, time)  // 25
  let distance = @azimuth.multiply(gravity, time_squared) / 2
  
  @azimuth.assert_eq(125, distance)  // 10 × 25 / 2 = 125
}

test "statistical_variance" {
  // 统计方差计算（简化版）
  // 数据集：10, 20, 30, 40, 50
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)  // 150
  let mean = sum / 5  // 30
  
  // 计算平方差之和
  let diff1 = @azimuth.add(10, -mean)  // -20
  let diff2 = @azimuth.add(20, -mean)  // -10
  let diff3 = @azimuth.add(30, -mean)  // 0
  let diff4 = @azimuth.add(40, -mean)  // 10
  let diff5 = @azimuth.add(50, -mean)  // 20
  
  let sum_squared_diffs = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1),    // 400
    @azimuth.multiply(diff2, diff2)),   // 100
    @azimuth.multiply(diff3, diff3)),   // 0
    @azimuth.multiply(diff4, diff4)),   // 100
    @azimuth.multiply(diff5, diff5))    // 400
  
  let variance = sum_squared_diffs / 5
  
  @azimuth.assert_eq(1000, sum_squared_diffs)  // 400 + 100 + 0 + 100 + 400 = 1000
  @azimuth.assert_eq(200, variance)            // 1000 / 5 = 200
}

test "game_mechanics" {
  // 游戏机制测试：经验值计算
  let base_exp = 100
  let level = 5
  let difficulty_multiplier = 2
  let bonus = 50
  
  // 计算获得的经验值
  let level_exp = @azimuth.multiply(base_exp, level)
  let adjusted_exp = @azimuth.multiply(level_exp, difficulty_multiplier)
  let total_exp = @azimuth.add(adjusted_exp, bonus)
  
  @azimuth.assert_eq(1050, total_exp)  // (100 × 5 × 2) + 50 = 1050
}

test "array_manipulation" {
  // 数组操作测试：模拟数组操作
  let array_size = 10
  let start_index = 2
  let end_index = 7
  
  // 计算数组长度
  let length = @azimuth.add(end_index, -start_index)
  @azimuth.assert_eq(5, length)  // 7 - 2 = 5
  
  // 计算中间索引
  let middle_index = @azimuth.add(start_index, length / 2)
  @azimuth.assert_eq(4, middle_index)  // 2 + 5/2 = 2 + 2 = 4
  
  // 计算循环索引（模拟数组循环）
  let current_index = 9
  let next_index = @azimuth.add(current_index, 1) % array_size
  @azimuth.assert_eq(0, next_index)  // (9 + 1) % 10 = 0
}

test "time_zone_conversion" {
  // 时区转换测试（简化版）
  let local_hour = 15  // 下午3点
  let time_zone_offset = -8  // 时差（小时）
  
  // 计算目标时间
  let target_hour = @azimuth.add(local_hour, time_zone_offset)
  
  // 处理跨日情况
  let adjusted_hour = if (target_hour < 0) {
    @azimuth.add(target_hour, 24)
  } else if (target_hour >= 24) {
    @azimuth.add(target_hour, -24)
  } else {
    target_hour
  }
  
  @azimuth.assert_eq(7, adjusted_hour)  // 15 + (-8) = 7（上午7点）
}

test "encryption_simulation" {
  // 简单加密模拟（凯撒密码）
  let original = 5
  let shift = 3
  let alphabet_size = 26
  
  // 加密
  let encrypted = @azimuth.add(original, shift) % alphabet_size
  @azimuth.assert_eq(8, encrypted)  // (5 + 3) % 26 = 8
  
  // 解密
  let decrypted = @azimuth.add(encrypted, @azimuth.add(alphabet_size, -shift)) % alphabet_size
  @azimuth.assert_eq(5, decrypted)  // (8 + (26-3)) % 26 = 5
}