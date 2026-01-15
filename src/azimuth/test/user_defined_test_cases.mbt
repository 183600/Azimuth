// 用户自定义的 MoonBit 测试用例
// 包含多种实用场景和边界情况测试

test "number_system_conversion" {
  // 测试数字系统转换：二进制转十进制（简化版）
  // 二进制数 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  let bit3 = @azimuth.multiply(1, @azimuth.multiply(@azimuth.multiply(2, 2), 2))  // 1×2³ = 8
  let bit2 = @azimuth.multiply(0, @azimuth.multiply(2, 2))  // 0×2² = 0
  let bit1 = @azimuth.multiply(1, 2)  // 1×2¹ = 2
  let bit0 = @azimuth.multiply(1, 1)  // 1×2⁰ = 1
  
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(bit3, bit2), bit1), bit0)
  @azimuth.assert_eq(11, decimal_value)
  
  // 测试二进制数 1101 = 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let binary_1101 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(0, 2), 1))
  @azimuth.assert_eq(13, binary_1101)
}

test "physics_gravity_calculation" {
  // 测试物理计算：自由落体运动（简化版，忽略空气阻力）
  // 公式：h = v₀t + (1/2)gt²，简化为：h = v₀t + 5t²（g≈10m/s²）
  
  let initial_velocity = 5  // 初始速度 5m/s
  let time = 3  // 时间 3秒
  let gravity = 10  // 重力加速度≈10m/s²
  
  // 计算位移：h = v₀t + (1/2)gt²
  let velocity_part = @azimuth.multiply(initial_velocity, time)  // 5×3 = 15
  let gravity_part = @azimuth.multiply(@azimuth.multiply(gravity, time), time) / 2  // 10×3²/2 = 45
  let total_distance = @azimuth.add(velocity_part, gravity_part)  // 15 + 45 = 60
  
  @azimuth.assert_eq(60, total_distance)
  
  // 测试从静止开始的自由落体
  let rest_distance = @azimuth.multiply(@azimuth.multiply(gravity, 2), 2) / 2  // 10×2²/2 = 20
  @azimuth.assert_eq(20, rest_distance)
}

test "array_index_calculation" {
  // 测试数组索引计算：二维数组转一维数组的索引计算
  let row = 2
  let col = 3
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, width), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, width), last_col)
  @azimuth.assert_eq(49, last_index)
}

test "game_damage_calculation" {
  // 测试游戏伤害计算：包含暴击和防御
  let base_damage = 100
  let attack_power = 50
  let critical_rate = 2  // 暴击倍数
  let defense = 20
  
  // 计算基础攻击力
  let attack_damage = @azimuth.add(base_damage, attack_power)  // 150
  
  // 计算暴击伤害
  let critical_damage = @azimuth.multiply(attack_damage, critical_rate)  // 300
  
  // 计算实际伤害（攻击力 - 防御）
  let actual_damage = @azimuth.add(critical_damage, -defense)  // 280
  
  @azimuth.assert_eq(280, actual_damage)
  
  // 测试未暴击情况
  let normal_damage = @azimuth.add(attack_damage, -defense)  // 130
  @azimuth.assert_eq(130, normal_damage)
}

test "bank_interest_calculation" {
  // 测试银行利息计算：单利和复利
  let principal = 10000
  let annual_rate = 5  // 5%年利率
  let years = 3
  
  // 单利计算：总金额 = 本金 + 本金×利率×年数
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years)  // 10000×5×3 = 150000
  let simple_total = @azimuth.add(principal, simple_interest)  // 10000 + 150000 = 160000
  
  @azimuth.assert_eq(160000, simple_total)
  
  // 复利计算（简化版）：总金额 = 本金×(1+利率×年数)
  let compound_factor = @azimuth.add(1, @azimuth.multiply(annual_rate, years))  // 1 + 5×3 = 16
  let compound_total = @azimuth.multiply(principal, compound_factor)  // 10000×16 = 160000
  
  @azimuth.assert_eq(160000, compound_total)
}

test "date_calculation" {
  // 测试日期计算：计算两个日期之间的天数（简化版）
  let year1 = 2023
  let month1 = 1
  let day1 = 15
  
  let year2 = 2023
  let month2 = 3
  let day2 = 20
  
  // 简化计算：假设每月30天
  let days_year1 = @azimuth.add(@azimuth.multiply(year1, 360), @azimuth.add(@azimuth.multiply(month1, 30), day1))
  let days_year2 = @azimuth.add(@azimuth.multiply(year2, 360), @azimuth.add(@azimuth.multiply(month2, 30), day2))
  
  let days_between = @azimuth.add(days_year2, -days_year1)
  
  // 2023年3月20日 - 2023年1月15日 = (2×30+20) - (0×30+15) = 80-15 = 65天
  @azimuth.assert_eq(65, days_between)
}

test "sorting_algorithm_simulation" {
  // 测试排序算法模拟：冒泡排序的交换次数计算
  // 数组：[5, 3, 8, 1, 2]
  // 冒泡排序需要的交换次数
  
  let array_size = 5
  let comparisons = @azimuth.multiply(array_size, @azimuth.add(array_size, -1)) / 2  // 5×4/2 = 10
  
  // 简化的交换次数计算（实际取决于数组内容）
  let estimated_swaps = comparisons / 2  // 假设一半需要交换
  @azimuth.assert_eq(5, estimated_swaps)
  
  // 测试选择算法复杂度：O(n²)
  let n = 10
  let operations = @azimuth.multiply(n, n)  // n² = 100
  @azimuth.assert_eq(100, operations)
}

test "string_length_calculation" {
  // 测试字符串长度计算（通过拼接操作间接计算）
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "ThisIsAVeryLongString"
  
  // 通过字符串拼接测试不同长度字符串的处理
  let short_greeting = @azimuth.greet(short_string)
  let medium_greeting = @azimuth.greet(medium_string)
  let long_greeting = @azimuth.greet(long_string)
  
  @azimuth.assert_eq_string("Hello, Hi!", short_greeting)
  @azimuth.assert_eq_string("Hello, MoonBit!", medium_greeting)
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongString!", long_greeting)
  
  // 测试空字符串和单字符
  let empty_greeting = @azimuth.greet("")
  let single_char_greeting = @azimuth.greet("A")
  
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
  @azimuth.assert_eq_string("Hello, A!", single_char_greeting)
}

test "probability_calculation" {
  // 测试概率计算：掷骰子的期望值
  let sides = 6  // 六面骰子
  
  // 计算期望值：(1+2+3+4+5+6)/6 = 21/6 = 3.5，简化为整数3
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6)
  let expected_value = sum / sides
  
  @azimuth.assert_eq(3, expected_value)  // 整数除法舍去小数部分
  
  // 计算两个骰子的和的期望值
  let two_dice_expected = @azimuth.multiply(2, expected_value)
  @azimuth.assert_eq(6, two_dice_expected)
  
  // 计算特定概率（简化）：掷出7的概率（1,6;2,5;3,4;4,3;5,2;6,1）= 6/36 = 1/6
  let total_outcomes = @azimuth.multiply(sides, sides)  // 36
  let favorable_outcomes = 6
  let probability = @azimuth.multiply(favorable_outcomes, 100) / total_outcomes  // 转换为百分比
  @azimuth.assert_eq(16, probability)  // 16%（整数除法）
}

test "circular_buffer_calculation" {
  // 测试循环缓冲区计算：环形数组的索引计算
  let buffer_size = 10
  let current_index = 8
  let steps_forward = 5
  let steps_backward = 3
  
  // 计算前向索引：(current_index + steps) % buffer_size
  let forward_index_raw = @azimuth.add(current_index, steps_forward)  // 8 + 5 = 13
  let forward_index = forward_index_raw % buffer_size  // 13 % 10 = 3
  
  @azimuth.assert_eq(3, forward_index)
  
  // 计算后向索引：(current_index - steps + buffer_size) % buffer_size
  let backward_index_raw = @azimuth.add(@azimuth.add(current_index, -steps_backward), buffer_size)  // 8 - 3 + 10 = 15
  let backward_index = backward_index_raw % buffer_size  // 15 % 10 = 5
  
  @azimuth.assert_eq(5, backward_index)
  
  // 测试边界情况
  let edge_forward = @azimuth.add(9, 1) % 10  // (9+1)%10 = 0
  @azimuth.assert_eq(0, edge_forward)
  
  let edge_backward = (@azimuth.add(0, -1) + 10) % 10  // (0-1+10)%10 = 9
  @azimuth.assert_eq(9, edge_backward)
}