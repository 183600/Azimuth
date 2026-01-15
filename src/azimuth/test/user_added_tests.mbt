// 新增的 MoonBit 测试用例
// 包含10个不同的测试场景，覆盖各种应用领域

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  // 验证计算结果
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  @azimuth.assert_eq(34, f9)
  @azimuth.assert_eq(55, f10)
  
  // 计算前10项的和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), f4)
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f5, f6), f7), f8), f9)
  let sum_first_10 = @azimuth.add(sum1, sum2)
  @azimuth.assert_eq(88, sum_first_10)
}

test "time_difference_calculation" {
  // 测试时间差计算
  // 计算两个时间点之间的差值
  
  // 时间点1：14:30
  let hour1 = 14
  let minute1 = 30
  let total_minutes1 = @azimuth.add(@azimuth.multiply(hour1, 60), minute1)  // 870
  
  // 时间点2：18:45
  let hour2 = 18
  let minute2 = 45
  let total_minutes2 = @azimuth.add(@azimuth.multiply(hour2, 60), minute2)  // 1125
  
  // 计算时间差（分钟）
  let time_diff = @azimuth.add(total_minutes2, -total_minutes1)  // 255分钟
  
  // 转换为小时和分钟
  let diff_hours = time_diff / 60  // 4小时
  let diff_minutes = time_diff % 60  // 15分钟
  
  @azimuth.assert_eq(4, diff_hours)
  @azimuth.assert_eq(15, diff_minutes)
  @azimuth.assert_eq(255, time_diff)
  
  // 测试跨天情况：23:30 到次日 01:15
  let late_hour = 23
  let late_minute = 30
  let late_total = @azimuth.add(@azimuth.multiply(late_hour, 60), late_minute)  // 1410
  
  let early_hour = 1
  let early_minute = 15
  let early_total = @azimuth.add(@azimuth.multiply(early_hour, 60), early_minute)  // 75
  
  // 跨天时间差计算
  let cross_day_diff = @azimuth.add(@azimuth.add(1440, early_total), -late_total)  // 165分钟
  let cross_day_hours = cross_day_diff / 60  // 2小时
  let cross_day_minutes = cross_day_diff % 60  // 45分钟
  
  @azimuth.assert_eq(2, cross_day_hours)
  @azimuth.assert_eq(45, cross_day_minutes)
  @azimuth.assert_eq(165, cross_day_diff)
}

test "simple_encryption_simulation" {
  // 测试简单加密模拟
  // 使用数字替换进行简单的加密
  
  // 原始数字
  let num1 = 123
  let num2 = 456
  let num3 = 789
  
  // 加密：每个数字加100
  let encrypted1 = @azimuth.add(num1, 100)  // 223
  let encrypted2 = @azimuth.add(num2, 100)  // 556
  let encrypted3 = @azimuth.add(num3, 100)  // 889
  
  // 验证加密结果
  @azimuth.assert_eq(223, encrypted1)
  @azimuth.assert_eq(556, encrypted2)
  @azimuth.assert_eq(889, encrypted3)
  
  // 解密：加密后的数字减100
  let decrypted1 = @azimuth.add(encrypted1, -100)  // 123
  let decrypted2 = @azimuth.add(encrypted2, -100)  // 456
  let decrypted3 = @azimuth.add(encrypted3, -100)  // 789
  
  // 验证解密结果
  @azimuth.assert_eq(123, decrypted1)
  @azimuth.assert_eq(456, decrypted2)
  @azimuth.assert_eq(789, decrypted3)
  
  // 计算加密后数字的"校验和"
  let encryption_checksum = @azimuth.add(@azimuth.add(encrypted1, encrypted2), encrypted3)
  @azimuth.assert_eq(1668, encryption_checksum)  // 223 + 556 + 889 = 1668
}

test "array_index_calculation" {
  // 测试数组索引计算
  // 模拟二维数组在一维数组中的索引计算
  
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  
  // 计算一维数组索引：index = row * cols + col
  let array_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  @azimuth.assert_eq(13, array_index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, cols), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 3
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, cols), last_col)
  @azimuth.assert_eq(19, last_index)
  
  // 计算数组总大小
  let total_size = @azimuth.multiply(rows, cols)
  @azimuth.assert_eq(20, total_size)
  
  // 验证索引在有效范围内
  let index_valid = array_index >= 0 && array_index < total_size
  @azimuth.assert_true(index_valid)
}

test "percentage_change_calculation" {
  // 测试百分比变化计算
  // 计算数值变化的百分比
  
  let original_value = 200
  let new_value = 250
  
  // 计算变化量
  let change = @azimuth.add(new_value, -original_value)  // 50
  
  // 计算百分比变化（乘以100以获得百分比）
  let percentage_change = @azimuth.multiply(change, 100) / original_value  // 25%
  @azimuth.assert_eq(25, percentage_change)
  
  // 测试减少的情况
  let decrease_original = 300
  let decrease_new = 240
  let decrease_change = @azimuth.add(decrease_new, -decrease_original)  // -60
  let decrease_percentage = @azimuth.multiply(decrease_change, 100) / decrease_original  // -20%
  @azimuth.assert_eq(-20, decrease_percentage)
  
  // 测试翻倍情况
  let double_original = 100
  let double_new = 200
  let double_change = @azimuth.add(double_new, -double_original)  // 100
  let double_percentage = @azimuth.multiply(double_change, 100) / double_original  // 100%
  @azimuth.assert_eq(100, double_percentage)
  
  // 测试减半情况
  let half_original = 200
  let half_new = 100
  let half_change = @azimuth.add(half_new, -half_original)  // -100
  let half_percentage = @azimuth.multiply(half_change, 100) / half_original  // -50%
  @azimuth.assert_eq(-50, half_percentage)
}

test "physics_formula_simulation" {
  // 测试物理公式模拟
  // 计算自由落体运动：h = v₀t + 0.5gt²
  // 简化计算：h = v₀t + 3t²（使用3代替0.5g，其中g≈9.8）
  
  let initial_velocity = 10  // 初速度 v₀ = 10 m/s
  let gravity = 3  // 简化的重力加速度系数
  let time1 = 2  // 时间 t = 2s
  let time2 = 5  // 时间 t = 5s
  
  // 计算2秒后的高度
  let height_part1 = @azimuth.multiply(initial_velocity, time1)  // v₀t = 20
  let time1_squared = @azimuth.multiply(time1, time1)  // t² = 4
  let height_part2 = @azimuth.multiply(gravity, time1_squared)  // 3t² = 12
  let total_height1 = @azimuth.add(height_part1, height_part2)  // h = 32
  @azimuth.assert_eq(32, total_height1)
  
  // 计算5秒后的高度
  let height_part3 = @azimuth.multiply(initial_velocity, time2)  // v₀t = 50
  let time2_squared = @azimuth.multiply(time2, time2)  // t² = 25
  let height_part4 = @azimuth.multiply(gravity, time2_squared)  // 3t² = 75
  let total_height2 = @azimuth.add(height_part3, height_part4)  // h = 125
  @azimuth.assert_eq(125, total_height2)
  
  // 计算速度变化：v = v₀ + gt
  // 简化：v = v₀ + 6t（使用6代替g）
  let gravity_velocity = 6  // 简化的重力加速度
  let velocity1 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity_velocity, time1))  // v = 22
  let velocity2 = @azimuth.add(initial_velocity, @azimuth.multiply(gravity_velocity, time2))  // v = 40
  
  @azimuth.assert_eq(22, velocity1)
  @azimuth.assert_eq(40, velocity2)
}

test "data_formatting_scenarios" {
  // 测试数据格式化场景
  // 模拟各种数据格式化需求
  
  // 计算电话号码的数字和（用于验证）
  let phone_part1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 8), 1), 2)
  let phone_part2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(3, 4), 5), 6), 7)
  let phone_sum = @azimuth.add(phone_part1, phone_part2)
  @azimuth.assert_eq(45, phone_sum)
  
  // 日期格式化：2023-12-25
  let year = 2023
  let day = 25
  
  // 计算年份是否为闰年（简化判断）
  let is_leap_year = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
  @azimuth.assert_false(is_leap_year)  // 2023不是闰年
  
  // 计算该日期是该年的第几天（简化计算）
  let days_part1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(0, 31), 28), 31), 30), 31)
  let days_part2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(30, 31), 31), 30), 30), 31)
  let days_before_month = @azimuth.add(days_part1, days_part2)  // 前11个月的总天数
  let day_of_year = @azimuth.add(days_before_month, day)  // 359
  @azimuth.assert_eq(359, day_of_year)
  
  // 货币格式化：12345 -> ¥12,345.00
  let amount = 12345
  let yuan_part = amount / 100  // 123
  let jiao_part = (amount % 100) / 10  // 4
  let fen_part = amount % 10  // 5
  
  @azimuth.assert_eq(123, yuan_part)
  @azimuth.assert_eq(4, jiao_part)
  @azimuth.assert_eq(5, fen_part)
  
  // 计算总金额（分）
  let yuan_total = @azimuth.multiply(yuan_part, 100)
  let jiao_total = @azimuth.multiply(jiao_part, 10)
  let total_fen = @azimuth.add(@azimuth.add(yuan_total, jiao_total), fen_part)
  @azimuth.assert_eq(12345, total_fen)
}

test "game_mechanics_simulation" {
  // 测试游戏机制模拟
  // 模拟RPG游戏的战斗计算
  
  let player_level = 15
  let enemy_level = 12
  let base_damage = 100
  let weapon_damage = 50
  let defense = 30
  
  // 计算等级差
  let level_diff = @azimuth.add(player_level, -enemy_level)  // 3
  @azimuth.assert_eq(3, level_diff)
  
  // 计算基础攻击力
  let level_bonus = @azimuth.multiply(level_diff, 10)
  let base_attack = @azimuth.add(base_damage, level_bonus)  // 100 + 3*10 = 130
  @azimuth.assert_eq(130, base_attack)
  
  // 计算总攻击力
  let total_attack = @azimuth.add(base_attack, weapon_damage)  // 130 + 50 = 180
  @azimuth.assert_eq(180, total_attack)
  
  // 计算暴击伤害（简化：2倍伤害）
  let critical_damage = @azimuth.multiply(total_attack, 2)  // 360
  @azimuth.assert_eq(360, critical_damage)
  
  // 计算实际伤害（攻击力 - 防御）
  let normal_damage = @azimuth.add(total_attack, -defense)  // 180 - 30 = 150
  let critical_actual_damage = @azimuth.add(critical_damage, -defense)  // 360 - 30 = 330
  
  @azimuth.assert_eq(150, normal_damage)
  @azimuth.assert_eq(330, critical_actual_damage)
  
  // 计算期望伤害（考虑暴击率）
  let normal_part = @azimuth.multiply(normal_damage, 80)
  let critical_part = @azimuth.multiply(critical_actual_damage, 20)
  let expected_damage = @azimuth.add(normal_part, critical_part) / 100
  @azimuth.assert_eq(186, expected_damage)  // (150*80 + 330*20) / 100 = 186
  
  // 计算击败敌人所需的攻击次数
  let enemy_hp = 1000
  let attacks_needed = enemy_hp / expected_damage + 1  // 向上取整
  @azimuth.assert_eq(6, attacks_needed)  // 1000/186 + 1 = 5.37 + 1 = 6.37 -> 6
}

test "simple_compression_simulation" {
  // 测试简单压缩模拟
  // 模拟行程长度编码（RLE）压缩
  
  // 统计各字符的出现次数
  let count_A = 4  // AAAA
  let count_B = 3  // BBB
  let count_C = 2  // CC
  let count_D = 1  // D
  let count_A2 = 2  // AA
  
  // 计算原始数据长度
  let len_part1 = @azimuth.add(@azimuth.add(@azimuth.add(count_A, count_B), count_C), count_D)
  let original_length = @azimuth.add(len_part1, count_A2)
  @azimuth.assert_eq(12, original_length)
  
  // 计算压缩后数据长度（每个字符用数字+字符表示，算作2个单位）
  let unique_chars = 5  // A, B, C, D, A(再次出现)
  let compressed_length = @azimuth.multiply(unique_chars, 2)
  @azimuth.assert_eq(10, compressed_length)
  
  // 计算压缩率
  let compression_diff = @azimuth.add(compressed_length, -original_length)
  let compression_ratio = @azimuth.multiply(compression_diff, 100) / original_length
  @azimuth.assert_eq(-16, compression_ratio)  // (10-12)*100/12 = -16.67% -> -16%
  
  // 模拟解压缩过程
  let decompressed_length = original_length
  @azimuth.assert_eq(original_length, decompressed_length)
  
  // 计算压缩节省的空间
  let space_saved = @azimuth.add(original_length, -compressed_length)
  @azimuth.assert_eq(2, space_saved)
  
  // 验证压缩效果
  let compression_effective = space_saved > 0
  @azimuth.assert_true(compression_effective)
  
  // 计算不同重复模式下的压缩效果
  let highly_repetitive_original = 20  // AAAAAAAAAAAAAAAAAAAA
  let highly_repetitive_compressed = 2  // 20A
  let high_compression_diff = @azimuth.add(highly_repetitive_compressed, -highly_repetitive_original)
  let high_compression_ratio = @azimuth.multiply(high_compression_diff, 100) / highly_repetitive_original
  @azimuth.assert_eq(-90, high_compression_ratio)  // (2-20)*100/20 = -90%
}

test "temperature_conversion_advanced" {
  // 测试高级温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化计算：F = C × 2 + 30（避免小数）
  
  // 测试常见温度点
  let freezing_celsius = 0    // 冰点
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20       // 室温
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100   // 沸点
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
  
  // 测试极端温度
  let extreme_celsius = -40   // 极冷
  let extreme_fahrenheit = @azimuth.add(@azimuth.multiply(extreme_celsius, 2), 30)
  @azimuth.assert_eq(-50, extreme_fahrenheit)
  
  // 反向计算验证（华氏度转摄氏度）
  // 简化公式：C = (F - 30) / 2
  let test_fahrenheit = 86
  let test_celsius = @azimuth.add(test_fahrenheit, -30) / 2
  @azimuth.assert_eq(28, test_celsius)  // (86-30)/2 = 28
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 计算等差数列的和：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  
  let n = 10
  let first = 1
  let last = 19
  
  // 计算和
  let sum_formula = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum_formula)  // 10 × (1 + 19) / 2 = 100
  
  // 手动验证前几项
  let manual_part1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  let manual_part2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(11, 13), 15), 17), 19)
  let manual_sum = @azimuth.add(manual_part1, manual_part2)
  @azimuth.assert_eq(100, manual_sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16 + 32
  // 使用公式：a × (r^n - 1) / (r - 1)
  let a = 2  // 首项
  let r = 3  // 公比
  let terms = 5  // 项数
  
  // 手动计算
  let geo_term1 = 2
  let geo_term2 = 4
  let geo_term3 = 8
  let geo_term4 = 16
  let geo_term5 = 32
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo_term1, geo_term2), geo_term3), geo_term4), geo_term5)
  @azimuth.assert_eq(62, geo_sum)
  
  // 验证平方数列的和：1² + 2² + 3² + 4² + 5²
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, square_sum)
}