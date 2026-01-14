// 新增的 MoonBit 测试用例
// 这些测试用例专注于不同的场景和边界情况

test "arithmetic_progression_sum" {
  // 测试等差数列求和
  // 计算前n个自然数的和：1 + 2 + 3 + ... + n
  // 使用整数除法
  let n = 10
  
  // 手动计算 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(3, 4)
  let sum3 = @azimuth.add(5, 6)
  let sum4 = @azimuth.add(7, 8)
  let sum5 = @azimuth.add(9, 10)
  
  let partial_sum1 = @azimuth.add(sum1, sum2)
  let partial_sum2 = @azimuth.add(sum3, sum4)
  let total_sum = @azimuth.add(@azimuth.add(partial_sum1, partial_sum2), sum5)
  
  @azimuth.assert_eq(55, total_sum)
}

test "geometric_progression_product" {
  // 测试等比数列求积
  // 计算 2 × 4 × 8 × 16
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  let product1 = @azimuth.multiply(term1, term2)
  let product2 = @azimuth.multiply(term3, term4)
  let final_product = @azimuth.multiply(product1, product2)
  
  @azimuth.assert_eq(1024, final_product)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, AB!", @azimuth.greet("AB"))
  
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, tab\tcharacter!", @azimuth.greet("tab\tcharacter"))
  @azimuth.assert_eq_string("Hello, quote\"character!", @azimuth.greet("quote\"character"))
  @azimuth.assert_eq_string("Hello, backslash\\character!", @azimuth.greet("backslash\\character"))
}

test "calculation_with_negative_intermediate_results" {
  // 测试包含负数中间结果的计算
  // (-5 + 3) * (-2) = 4
  let intermediate = @azimuth.add(-5, 3)  // -2
  let result = @azimuth.multiply(intermediate, -2)  // 4
  @azimuth.assert_eq(4, result)
  
  // (10 + (-15)) * (-3) = 15
  let intermediate2 = @azimuth.add(10, -15)  // -5
  let result2 = @azimuth.multiply(intermediate2, -3)  // 15
  @azimuth.assert_eq(15, result2)
}

test "extreme_boundary_operations" {
  // 测试极值边界操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与-1的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与1的运算
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试0与极值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算
  // 计算贷款月供（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  let months = @azimuth.multiply(years, 12)  // 总月数
  
  // 简化的月供计算（仅用于测试）
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(277, monthly_payment)  // 简化计算结果
}

test "physics_simulation" {
  // 测试物理模拟计算
  // 自由落体距离：s = 5 * t^2 (g ≈ 10 m/s²)
  let time = 3
  let distance = @azimuth.multiply(5, @azimuth.multiply(time, time))
  @azimuth.assert_eq(45, distance)  // 5 * 3² = 45
  
  // 动能计算：E = m * v^2 / 2
  let mass = 10
  let velocity = 5
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(125, kinetic_energy)  // 10 * 5² / 2 = 125
}

test "statistical_analysis" {
  // 测试统计分析计算
  // 计算平均值
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  let sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(data1, data2), 
      @azimuth.add(data3, data4)
    ), 
    data5
  )
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)
  
  // 计算方差（简化版）
  let diff1 = data1 - mean
  let diff2 = data2 - mean
  let diff3 = data3 - mean
  let diff4 = data4 - mean
  let diff5 = data5 - mean
  
  let variance_sum = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(diff1, diff1),
      @azimuth.multiply(diff2, diff2)
    ),
    @azimuth.add(
      @azimuth.add(
        @azimuth.multiply(diff3, diff3),
        @azimuth.multiply(diff4, diff4)
      ),
      @azimuth.multiply(diff5, diff5)
    )
  )
  let variance = variance_sum / 5
  @azimuth.assert_eq(200, variance)  // ((-20)² + (-10)² + 0² + 10² + 20²) / 5 = (400+100+0+100+400)/5 = 1000/5 = 200
}

test "game_mechanics" {
  // 测试游戏机制计算
  // 计算经验值和等级
  let base_exp = 100
  let level_multiplier = 15
  let current_level = 5
  
  // 计算升级所需经验值
  let exp_needed = @azimuth.multiply(base_exp, @azimuth.multiply(current_level, level_multiplier))
  @azimuth.assert_eq(7500, exp_needed)  // 100 * 5 * 15 = 7500
  
  // 计算伤害值：基础伤害 * (1 + 力量加成)
  let base_damage = 50
  let strength_bonus = 20  // 20%加成
  let total_damage = @azimuth.add(base_damage, @azimuth.multiply(base_damage, strength_bonus) / 100)
  @azimuth.assert_eq(60, total_damage)  // 50 + 50*20/100 = 50 + 10 = 60
  
  // 计算暴击伤害
  let crit_multiplier = 2
  let crit_damage = @azimuth.multiply(total_damage, crit_multiplier)
  @azimuth.assert_eq(120, crit_damage)  // 60 * 2 = 120
}

test "cryptography_simulation" {
  // 测试密码学模拟（简化版）
  // 简单的凯撒密码
  let original = 13  // 原始数字
  let shift = 3      // 偏移量
  
  // 加密：(original + shift) mod alphabet_size
  let encrypted = @azimuth.add(original, shift)
  @azimuth.assert_eq(16, encrypted)  // (13 + 3) mod 26 = 16
  
  // 解密：(encrypted - shift) mod alphabet_size
  let decrypted = encrypted - shift
  @azimuth.assert_eq(13, decrypted)  // (16 - 3) mod 26 = 13
  
  // 简单的哈希函数模拟
  let input = 12345
  let hash_prime = 31
  
  // hash = (input * prime) mod mod
  let hash_result = (@azimuth.multiply(input, hash_prime)) % 1000
  @azimuth.assert_eq(695, hash_result)  // (12345 * 31) mod 1000 = 382695 mod 1000 = 695
}