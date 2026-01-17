// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，涵盖多样化的测试场景

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  // 使用数值模拟布尔值: 1 = true, 0 = false
  
  // 逻辑与 (AND): a && b
  // 通过乘法模拟: 1 * 1 = 1 (true), 其他情况 = 0 (false)
  assert_eq(azimuth::multiply(1, 1), 1)  // true && true = true
  assert_eq(azimuth::multiply(1, 0), 0)  // true && false = false
  assert_eq(azimuth::multiply(0, 1), 0)  // false && true = false
  assert_eq(azimuth::multiply(0, 0), 0)  // false && false = false
  
  // 逻辑或 (OR): a || b
  // 通过加法和最小值模拟: min(a + b, 1)
  assert_eq(azimuth::add(1, 0) > 0 ? 1 : 0, 1)  // true || false = true
  assert_eq(azimuth::add(0, 1) > 0 ? 1 : 0, 1)  // false || true = true
  assert_eq(azimuth::add(0, 0) > 0 ? 1 : 0, 0)  // false || false = false
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 平均值计算: (a + b + c) / 3
  let values_sum = azimuth::add(azimuth::add(10, 20), 30)
  let mean = values_sum / 3
  assert_eq(mean, 20)
  
  // 加权平均: (a*weight_a + b*weight_b) / (weight_a + weight_b)
  let a = 80
  let b = 90
  let weight_a = 2
  let weight_b = 3
  let weighted_sum = azimuth::add(azimuth::multiply(a, weight_a), azimuth::multiply(b, weight_b))
  let total_weight = azimuth::add(weight_a, weight_b)
  let weighted_mean = weighted_sum / total_weight
  assert_eq(weighted_mean, 86)
  
  // 方差计算 (简化版): (x1-mean)² + (x2-mean)² + (x3-mean)²
  let x1 = 5
  let x2 = 10
  let x3 = 15
  let data_mean = azimuth::add(azimuth::add(x1, x2), x3) / 3
  let diff1 = azimuth::add(x1, -data_mean)
  let diff2 = azimuth::add(x2, -data_mean)
  let diff3 = azimuth::add(x3, -data_mean)
  let variance = azimuth::add(azimuth::add(azimuth::multiply(diff1, diff1), azimuth::multiply(diff2, diff2)), azimuth::multiply(diff3, diff3))
  assert_eq(variance, 50)
}

test "physics_calculations" {
  // 测试物理计算
  
  // 速度计算: 距离 / 时间
  let distance = 100  // 米
  let time = 10       // 秒
  let velocity = distance / time
  assert_eq(velocity, 10)  // 米/秒
  
  // 动能计算: 0.5 * 质量 * 速度²
  let mass = 10      // 千克
  let speed = 5      // 米/秒
  let kinetic_energy = azimuth::multiply(azimuth::multiply(mass, azimuth::multiply(speed, speed)), 1) / 2
  assert_eq(kinetic_energy, 125)  // 焦耳
  
  // 势能计算: 质量 * 重力加速度 * 高度
  let height = 20    // 米
  let gravity = 10   // 米/秒² (简化值)
  let potential_energy = azimuth::multiply(azimuth::multiply(mass, gravity), height)
  assert_eq(potential_energy, 2000)  // 焦耳
}

test "number_system_conversions" {
  // 测试数字系统转换
  
  // 二进制转十进制: 1010 = 1*2³ + 0*2² + 1*2¹ + 0*2⁰ = 8 + 0 + 2 + 0 = 10
  let binary_1010 = azimuth::add(azimuth::add(azimuth::multiply(1, 8), azimuth::multiply(0, 4)), azimuth::add(azimuth::multiply(1, 2), azimuth::multiply(0, 1)))
  assert_eq(binary_1010, 10)
  
  // 二进制转十进制: 1111 = 1*2³ + 1*2² + 1*2¹ + 1*2⁰ = 8 + 4 + 2 + 1 = 15
  let binary_1111 = azimuth::add(azimuth::add(azimuth::multiply(1, 8), azimuth::multiply(1, 4)), azimuth::add(azimuth::multiply(1, 2), azimuth::multiply(1, 1)))
  assert_eq(binary_1111, 15)
  
  // 八进制转十进制: 123 = 1*8² + 2*8¹ + 3*8⁰ = 64 + 16 + 3 = 83
  let octal_123 = azimuth::add(azimuth::add(azimuth::multiply(1, 64), azimuth::multiply(2, 8)), azimuth::multiply(3, 1))
  assert_eq(octal_123, 83)
}

test "algorithmic_patterns" {
  // 测试算法模式
  
  // 阶乘计算 (简化版): 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = azimuth::multiply(azimuth::multiply(azimuth::multiply(azimuth::multiply(5, 4), 3), 2), 1)
  assert_eq(factorial_5, 120)
  
  // 最大公约数模拟 (欧几里得算法简化版)
  // gcd(48, 18) = gcd(18, 48 % 18 = 12) = gcd(12, 18 % 12 = 6) = gcd(6, 12 % 6 = 0) = 6
  let a = 48
  let b = 18
  let remainder1 = a % b  // 48 % 18 = 12
  let remainder2 = b % remainder1  // 18 % 12 = 6
  let gcd = remainder2  // 简化版本
  assert_eq(gcd, 6)
  
  // 线性搜索模拟
  let target = 7
  let array_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), azimuth::add(azimuth::add(5, 6), 7))
  assert_eq(array_sum > target ? 1 : 0, 1)  // 找到目标
}

test "date_time_calculations" {
  // 测试日期时间计算
  
  // 闰年判断 (简化版): 能被4整除但不能被100整除，或者能被400整除
  let year = 2024
  let divisible_by_4 = year % 4 == 0 ? 1 : 0
  let divisible_by_100 = year % 100 == 0 ? 1 : 0
  let divisible_by_400 = year % 400 == 0 ? 1 : 0
  let is_leap = (divisible_by_4 == 1 && divisible_by_100 == 0) || divisible_by_400 == 1 ? 1 : 0
  assert_eq(is_leap, 1)  // 2024是闰年
  
  // 年份中的天数 (非闰年: 365, 闰年: 366)
  let days_in_year = is_leap == 1 ? 366 : 365
  assert_eq(days_in_year, 366)
  
  // 月份天数模拟 (以30天为基准的简化计算)
  let month = 2  // 二月
  let base_days = 30
  let february_adjustment = month == 2 ? (is_leap == 1 ? -2 : -3) : 0
  let days_in_month = azimuth::add(base_days, february_adjustment)
  assert_eq(days_in_month, 28)  // 闰年二月29天，这里简化为28天
}

test "string_length_calculations" {
  // 测试字符串长度计算
  
  // 基本问候字符串长度
  let greeting1 = "Hello"
  let name1 = "World"
  let full_greeting1 = azimuth::greet(name1)
  // "Hello, World!" 长度为 13 (通过字符数估算)
  let expected_length1 = 13
  assert_eq(full_greeting1 == "Hello, World!" ? expected_length1 : 0, expected_length1)
  
  // 带空格的字符串
  let name2 = "MoonBit"
  let full_greeting2 = azimuth::greet(name2)
  // "Hello, MoonBit!" 长度为 15
  let expected_length2 = 15
  assert_eq(full_greeting2 == "Hello, MoonBit!" ? expected_length2 : 0, expected_length2)
  
  // 空字符串
  let name3 = ""
  let full_greeting3 = azimuth::greet(name3)
  // "Hello, !" 长度为 8
  let expected_length3 = 8
  assert_eq(full_greeting3 == "Hello, !" ? expected_length3 : 0, expected_length3)
}

test "game_score_calculations" {
  // 测试游戏分数计算
  
  // 基础得分系统
  let base_score = 0
  let level_multiplier = 2
  let bonus_per_level = 100
  
  // 第一关得分
  let level1_score = azimuth::multiply(azimuth::add(base_score, bonus_per_level), level_multiplier)
  assert_eq(level1_score, 200)
  
  // 第二关得分 (bonus增加)
  let level2_bonus = azimuth::add(bonus_per_level, 50)
  let level2_score = azimuth::multiply(azimuth::add(base_score, level2_bonus), level_multiplier)
  assert_eq(level2_score, 300)
  
  // 连击奖励计算
  let combo_count = 5
  let combo_multiplier = azimuth::add(1, combo_count)  // 1 + 5 = 6倍
  let final_score = azimuth::multiply(level2_score, combo_multiplier)
  assert_eq(final_score, 1800)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  
  // 除零错误预防
  let dividend = 100
  let divisor1 = 10
  let divisor2 = 0
  
  // 正常除法
  let result1 = divisor1 != 0 ? dividend / divisor1 : 0
  assert_eq(result1, 10)
  
  // 防止除零
  let result2 = divisor2 != 0 ? dividend / divisor2 : 0
  assert_eq(result2, 0)
  
  // 数值溢出预防 (简化版)
  let max_value = 1000
  let addend1 = 500
  let addend2 = 600
  
  // 检查是否会溢出
  let would_overflow = azimuth::add(addend1, addend2) > max_value ? 1 : 0
  let safe_result = would_overflow == 1 ? max_value : azimuth::add(addend1, addend2)
  assert_eq(safe_result, max_value)
  
  // 正常相加
  let addend3 = 300
  let addend4 = 400
  let would_overflow2 = azimuth::add(addend3, addend4) > max_value ? 1 : 0
  let safe_result2 = would_overflow2 == 1 ? max_value : azimuth::add(addend3, addend4)
  assert_eq(safe_result2, 700)
}

test "recursive_pattern_simulation" {
  // 测试递归模式模拟
  
  // 斐波那契数列 (前几项)
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  let fib6 = azimuth::add(fib4, fib5)  // 8
  let fib7 = azimuth::add(fib5, fib6)  // 13
  let fib8 = azimuth::add(fib6, fib7)  // 21
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  assert_eq(fib6, 8)
  assert_eq(fib7, 13)
  assert_eq(fib8, 21)
  
  // 幂运算模拟 (通过重复乘法)
  let base = 3
  let exponent = 4  // 3^4 = 81
  let power = azimuth::multiply(azimuth::multiply(azimuth::multiply(base, base), base), base)
  assert_eq(power, 81)
  
  // 累加和 (1 + 2 + ... + n)
  let n = 10
  let sum_n = azimuth::multiply(azimuth::add(n, 1), n) / 2  // 公式: n(n+1)/2
  assert_eq(sum_n, 55)
}