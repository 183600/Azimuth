// 增强的 MoonBit 测试用例
// 这些测试用例涵盖了更多实际应用场景和复杂的计算逻辑

// 导入主模块中的函数
use azimuth

test "prime_number_calculation" {
  // 质数检测相关的计算
  // 检查一个数是否为质数的简单算法（使用试除法）
  let num = 17
  
  // 检查17是否能被2到4之间的数整除
  let divisible_by_2 = num % 2 == 0
  let divisible_by_3 = num % 3 == 0
  let divisible_by_4 = num % 4 == 0
  
  // 17是质数，所以不应该被这些数整除
  assert_false(divisible_by_2)
  assert_false(divisible_by_3)
  assert_false(divisible_by_4)
  
  // 测试非质数
  let non_prime = 15
  let divisible_by_3_15 = non_prime % 3 == 0
  let divisible_by_5_15 = non_prime % 5 == 0
  
  assert_true(divisible_by_3_15)
  assert_true(divisible_by_5_15)
}

test "fibonacci_sequence" {
  // 斐波那契数列计算
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)  // 1
  let f3 = add(f2, f1)  // 2
  let f4 = add(f3, f2)  // 3
  let f5 = add(f4, f3)  // 5
  let f6 = add(f5, f4)  // 8
  let f7 = add(f6, f5)  // 13
  let f8 = add(f7, f6)  // 21
  let f9 = add(f8, f7)  // 34
  
  assert_eq(0, f0)
  assert_eq(1, f1)
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  assert_eq(34, f9)
}

test "string_unicode_processing" {
  // Unicode 字符串处理测试
  let chinese_text = "你好世界"
  let japanese_text = "こんにちは"
  let emoji_text = "🌍🚀💻"
  let mixed_text = "Hello世界🌍"
  
  // 测试包含不同语言字符的字符串
  assert_eq_string("Hello, 你好世界!", greet(chinese_text))
  assert_eq_string("Hello, こんにちは!", greet(japanese_text))
  assert_eq_string("Hello, 🌍🚀💻!", greet(emoji_text))
  assert_eq_string("Hello, Hello世界🌍!", greet(mixed_text))
  
  // 测试包含特殊符号的字符串
  let symbols = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(symbols))
}

test "physics_kinematics" {
  // 物理运动学计算
  // 距离 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 计算距离：10 × 5 + 0.5 × 2 × 5² = 50 + 0.5 × 2 × 25 = 50 + 25 = 75
  let distance_part1 = multiply(initial_velocity, time)
  let distance_part2 = multiply(multiply(2, multiply(time, time)), 1) / 4  // 简化0.5 × 2 × 25
  let total_distance = add(distance_part1, distance_part2)
  
  assert_eq(75, total_distance)
  
  // 计算末速度：初速度 + 加速度 × 时间
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  assert_eq(20, final_velocity)  // 10 + 2 × 5 = 20
}

test "statistical_variance" {
  // 统计学方差计算
  // 方差 = Σ(xi - μ)² / n，其中μ是平均值
  
  let values = [10, 15, 20, 25, 30]
  let n = 5
  
  // 计算平均值：(10 + 15 + 20 + 25 + 30) / 5 = 100 / 5 = 20
  let sum = add(add(add(add(10, 15), 20), 25), 30)
  let mean = sum / n
  assert_eq(20, mean)
  
  // 计算方差：((10-20)² + (15-20)² + (20-20)² + (25-20)² + (30-20)²) / 5
  // = (100 + 25 + 0 + 25 + 100) / 5 = 250 / 5 = 50
  let diff1 = add(10, -20)  // -10
  let diff2 = add(15, -20)  // -5
  let diff3 = add(20, -20)  // 0
  let diff4 = add(25, -20)  // 5
  let diff5 = add(30, -20)  // 10
  
  let squared_diff1 = multiply(diff1, diff1)  // 100
  let squared_diff2 = multiply(diff2, diff2)  // 25
  let squared_diff3 = multiply(diff3, diff3)  // 0
  let squared_diff4 = multiply(diff4, diff4)  // 25
  let squared_diff5 = multiply(diff5, diff5)  // 100
  
  let variance_sum = add(add(add(add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5)
  let variance = variance_sum / n
  
  assert_eq(50, variance)
}

test "game_damage_calculation" {
  // 游戏伤害计算
  let base_damage = 100
  let weapon_multiplier = 150  // 1.5倍，用150表示
  let critical_chance = 20     // 20%暴击率
  let defense = 30
  
  // 基础伤害计算：100 × 1.5 = 150
  let weapon_damage = multiply(base_damage, weapon_multiplier) / 100
  assert_eq(150, weapon_damage)
  
  // 暴击伤害（简化计算）：150 × 2 = 300
  let critical_damage = multiply(weapon_damage, 200) / 100
  assert_eq(300, critical_damage)
  
  // 最终伤害：伤害 - 防御
  let final_damage = add(weapon_damage, -defense)
  assert_eq(120, final_damage)
  
  // 暴击最终伤害
  let critical_final_damage = add(critical_damage, -defense)
  assert_eq(270, critical_final_damage)
}

test "financial_depreciation" {
  // 金融折旧计算（直线折旧法）
  let asset_cost = 10000
  let salvage_value = 1000
  let useful_life = 5  // 年
  
  // 年折旧额 = (原值 - 残值) / 使用年限
  let depreciation_per_year = add(asset_cost, -salvage_value) / useful_life
  assert_eq(1800, depreciation_per_year)  // (10000 - 1000) / 5 = 1800
  
  // 3年后的累计折旧
  let accumulated_depreciation = multiply(depreciation_per_year, 3)
  assert_eq(5400, accumulated_depreciation)
  
  // 3年后的账面价值
  let book_value = add(asset_cost, -accumulated_depreciation)
  assert_eq(4600, book_value)
}

test "data_structure_hash_simulation" {
  // 数据结构哈希计算模拟
  let key1 = "user123"
  let key2 = "admin456"
  
  // 简单的哈希函数模拟：将字符的ASCII值相加
  // 这里我们使用字符串长度作为简化的哈希值
  let hash1 = 7  // "user123" 的长度
  let hash2 = 8  // "admin456" 的长度
  
  // 测试哈希值的不同
  assert_true(hash1 != hash2)
  
  // 测试相同字符串的哈希值相同
  let hash1_duplicate = 7
  assert_eq(hash1, hash1_duplicate)
  
  // 模拟哈希表索引计算：hash % table_size
  let table_size = 10
  let index1 = hash1 % table_size
  let index2 = hash2 % table_size
  
  assert_eq(7, index1)
  assert_eq(8, index2)
}

test "date_time_calculations" {
  // 日期时间计算
  let base_year = 2023
  let days_in_year = 365
  let days_in_leap_year = 366
  
  // 计算从2023年开始的天数
  let year_offset = 5
  let total_days = multiply(days_in_year, year_offset)
  assert_eq(1825, total_days)  // 365 × 5 = 1825
  
  // 考虑闰年（简化：每4年一个闰年）
  let leap_years = year_offset / 4
  let leap_days = multiply(leap_years, 1)
  let total_days_with_leap = add(total_days, leap_days)
  assert_eq(1826, total_days_with_leap)  // 1825 + 1 = 1826
  
  // 计算星期几（简化计算）
  let start_day = 0  // 0表示周日
  let day_offset = 100
  let current_day = add(start_day, day_offset) % 7
  assert_eq(2, current_day)  // 100 % 7 = 2，表示周二
}

test "encryption_caesar_cipher" {
  // 凯撒密码加密模拟
  let plaintext = "HELLO"
  let shift = 3
  
  // 简化的凯撒密码：只处理大写字母
  // H(7) + 3 = K(10), E(4) + 3 = H(7), L(11) + 3 = O(14), L(11) + 3 = O(14), O(14) + 3 = R(17)
  // 这里我们用数字表示字母位置
  let h_pos = 7
  let e_pos = 4
  let l_pos = 11
  let o_pos = 14
  
  let h_encrypted = add(h_pos, shift)
  let e_encrypted = add(e_pos, shift)
  let l_encrypted = add(l_pos, shift)
  let o_encrypted = add(o_pos, shift)
  
  assert_eq(10, h_encrypted)  // K
  assert_eq(7, e_encrypted)   // H
  assert_eq(14, l_encrypted)  // O
  assert_eq(17, o_encrypted)  // R
  
  // 测试解密（反向移位）
  let k_pos = 10
  let k_decrypted = add(k_pos, -shift)
  assert_eq(7, k_decrypted)  // 回到H
}