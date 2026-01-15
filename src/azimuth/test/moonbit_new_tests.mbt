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
  let sum_first_10 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), f4), f5), f6), f7), f8), f9)
  @azimuth.assert_eq(88, sum_first_10)
}

test "id_number_validation_simulation" {
  // 测试身份证号验证模拟
  // 简化版本：检查前6位地区码和后4位校验码
  
  // 模拟身份证号：11010519900307203X
  let area_code = 110105  // 地区码
  let birth_year = 1990   // 出生年份
  let birth_month = 3     // 出生月份
  let birth_day = 7       // 出生日
  let sequence_code = 203 // 顺序码
  let check_code = 88     // 校验码（X转换为88）
  
  // 验证地区码（在合理范围内）
  let min_area_code = 110000
  let max_area_code = 659004
  let area_valid = @azimuth.add(area_code, -min_area_code) >= 0 && @azimuth.add(max_area_code, -area_code) >= 0
  
  // 验证出生日期（简化：年份在1900-2023之间）
  let min_year = 1900
  let max_year = 2023
  let year_valid = @azimuth.add(birth_year, -min_year) >= 0 && @azimuth.add(max_year, -birth_year) >= 0
  
  // 验证月份（1-12）
  let month_valid = @azimuth.add(birth_month, -1) >= 0 && @azimuth.add(12, -birth_month) >= 0
  
  // 验证日期（1-31）
  let day_valid = @azimuth.add(birth_day, -1) >= 0 && @azimuth.add(31, -birth_day) >= 0
  
  // 计算校验和（简化算法）
  let part1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(1, 1), @azimuth.multiply(1, 2)), @azimuth.multiply(0, 3)), @azimuth.multiply(1, 4)), @azimuth.multiply(0, 5)), 
    @azimuth.multiply(5, 6)), @azimuth.multiply(1, 7)), @azimuth.multiply(9, 8)), @azimuth.multiply(9, 9)), @azimuth.multiply(0, 10)), @azimuth.multiply(0, 11))
  let part2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(3, 12)), @azimuth.multiply(0, 13)), @azimuth.multiply(7, 14)), @azimuth.multiply(2, 15)), 
    @azimuth.multiply(0, 16)), @azimuth.multiply(3, 17)), @azimuth.multiply(88, 18))
  let weighted_sum = @azimuth.add(part1, part2)
  
  // 验证校验和能被11整除
  let check_valid = weighted_sum % 11 == 0
  
  // 综合验证结果
  let all_valid = area_valid && year_valid && month_valid && day_valid && check_valid
  @azimuth.assert_true(all_valid)
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
  // 使用凯撒密码进行简单的字符加密
  
  // 原始消息
  let original_msg = "HELLO"
  
  // 模拟加密：每个字符的ASCII码值加3
  let H_code = 72  // H的ASCII码
  let E_code = 69  // E的ASCII码
  let L_code = 76  // L的ASCII码
  let O_code = 79  // O的ASCII码
  
  // 加密后的码值
  let H_encrypted = @azimuth.add(H_code, 3)  // 75 (K)
  let E_encrypted = @azimuth.add(E_code, 3)  // 72 (H)
  let L_encrypted = @azimuth.add(L_code, 3)  // 79 (O)
  let O_encrypted = @azimuth.add(O_code, 3)  // 82 (R)
  
  // 验证加密结果
  @azimuth.assert_eq(75, H_encrypted)
  @azimuth.assert_eq(72, E_encrypted)
  @azimuth.assert_eq(79, L_encrypted)
  @azimuth.assert_eq(82, O_encrypted)
  
  // 模拟解密：加密后的码值减3
  let H_decrypted = @azimuth.add(H_encrypted, -3)  // 72 (H)
  let E_decrypted = @azimuth.add(E_encrypted, -3)  // 69 (E)
  let L_decrypted = @azimuth.add(L_encrypted, -3)  // 76 (L)
  let O_decrypted = @azimuth.add(O_encrypted, -3)  // 79 (O)
  
  // 验证解密结果
  @azimuth.assert_eq(72, H_decrypted)
  @azimuth.assert_eq(69, E_decrypted)
  @azimuth.assert_eq(76, L_decrypted)
  @azimuth.assert_eq(79, O_decrypted)
  
  // 计算加密后字符串的"校验和"
  let encryption_checksum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(H_encrypted, E_encrypted), L_encrypted), L_encrypted), O_encrypted)
  @azimuth.assert_eq(391, encryption_checksum)
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
  let height_part2 = @azimuth.multiply(gravity, @azimuth.multiply(time1, time1))  // 3t² = 12
  let total_height1 = @azimuth.add(height_part1, height_part2)  // h = 32
  @azimuth.assert_eq(32, total_height1)
  
  // 计算5秒后的高度
  let height_part3 = @azimuth.multiply(initial_velocity, time2)  // v₀t = 50
  let height_part4 = @azimuth.multiply(gravity, @azimuth.multiply(time2, time2))  // 3t² = 75
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
  
  // 电话号码格式化：13812345678 -> 138-1234-5678
  let phone_prefix = 138
  let phone_middle = 1234
  let phone_suffix = 5678
  
  // 计算电话号码的数字和（用于验证）
  let phone_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    1, 3), 8), 1), 2), 3), 4), 5), 6), 7), 8), 0)
  @azimuth.assert_eq(45, phone_sum)
  
  // 日期格式化：2023-12-25
  let year = 2023
  let month = 12
  let day = 25
  
  // 计算年份是否为闰年（简化判断）
  let is_leap_year = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
  @azimuth.assert_false(is_leap_year)  // 2023不是闰年
  
  // 计算该日期是该年的第几天（简化计算）
  let days_before_month = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    0, 31), 28), 31), 30), 31), 30), 31), 31), 30), 30), 31)  // 前11个月的总天数
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
  let total_fen = @azimuth.add(@azimuth.add(@azimuth.multiply(yuan_part, 100), @azimuth.multiply(jiao_part, 10)), fen_part)
  @azimuth.assert_eq(12345, total_fen)
}

test "game_mechanics_simulation" {
  // 测试游戏机制模拟
  // 模拟RPG游戏的战斗计算
  
  let player_level = 15
  let enemy_level = 12
  let base_damage = 100
  let weapon_damage = 50
  let critical_rate = 20  // 20%暴击率
  let defense = 30
  
  // 计算等级差
  let level_diff = @azimuth.add(player_level, -enemy_level)  // 3
  @azimuth.assert_eq(3, level_diff)
  
  // 计算基础攻击力
  let base_attack = @azimuth.add(base_damage, @azimuth.multiply(level_diff, 10))  // 100 + 3*10 = 130
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
  let expected_damage = @azimuth.add(@azimuth.multiply(normal_damage, 80), @azimuth.multiply(critical_actual_damage, 20)) / 100
  @azimuth.assert_eq(186, expected_damage)  // (150*80 + 330*20) / 100 = 186
  
  // 计算击败敌人所需的攻击次数
  let enemy_hp = 1000
  let attacks_needed = enemy_hp / expected_damage + 1  // 向上取整
  @azimuth.assert_eq(6, attacks_needed)  // 1000/186 + 1 = 5.37 + 1 = 6.37 -> 6
}

test "simple_compression_simulation" {
  // 测试简单压缩模拟
  // 模拟行程长度编码（RLE）压缩
  
  // 原始数据：AAAABBBCCDAA
  // 压缩后：4A3B2C1D2A
  
  // 统计各字符的出现次数
  let count_A = 4  // AAAA
  let count_B = 3  // BBB
  let count_C = 2  // CC
  let count_D = 1  // D
  let count_A2 = 2  // AA
  
  // 计算原始数据长度
  let original_length = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(count_A, count_B), count_C), count_D), count_A2)
  @azimuth.assert_eq(12, original_length)
  
  // 计算压缩后数据长度（每个字符用数字+字符表示，算作2个单位）
  let unique_chars = 5  // A, B, C, D, A(再次出现)
  let compressed_length = @azimuth.multiply(unique_chars, 2)
  @azimuth.assert_eq(10, compressed_length)
  
  // 计算压缩率
  let compression_ratio = @azimuth.multiply(@azimuth.add(compressed_length, -original_length), 100) / original_length
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
  let high_compression_ratio = @azimuth.multiply(@azimuth.add(highly_repetitive_compressed, -highly_repetitive_original), 100) / highly_repetitive_original
  @azimuth.assert_eq(-90, high_compression_ratio)  // (2-20)*100/20 = -90%
}