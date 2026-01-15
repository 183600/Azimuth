// 增强的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能和边界情况

test "arithmetic_sequence_operations" {
  // 测试等差数列操作
  // 计算前n个自然数的和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
  
  // 计算前n个偶数的和：2 + 4 + 6 + ... + 2n = n × (n + 1)
  let even_sum = multiply(n, add(n, 1))
  assert_eq(110, even_sum)
}

test "geometric_sequence_operations" {
  // 测试等比数列操作
  // 计算2^0 + 2^1 + 2^2 + 2^3 + 2^4 = 31
  let term1 = 1
  let term2 = multiply(2, 1)
  let term3 = multiply(2, term2)
  let term4 = multiply(2, term3)
  let term5 = multiply(2, term4)
  let geo_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(31, geo_sum)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123ABC456"
  let result2 = greet(alphanumeric)
  assert_eq_string("Hello, User123ABC456!", result2)
  
  // 测试包含空格的字符串
  let spaced = "  leading and trailing  "
  let result3 = greet(spaced)
  assert_eq_string("Hello,   leading and trailing  !", result3)
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - C × 2/10 + 32
  
  let celsius = 25
  let approx_fahrenheit = add(add(multiply(celsius, 2), multiply(multiply(celsius, -2), 1) / 10), 32)
  assert_eq(77, approx_fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = add(add(multiply(freezing_c, 2), multiply(multiply(freezing_c, -2), 1) / 10), 32)
  assert_eq(32, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = add(add(multiply(boiling_c, 2), multiply(multiply(boiling_c, -2), 1) / 10), 32)
  assert_eq(212, boiling_f)
}

test "financial_compound_interest" {
  // 测试金融复利计算
  // 复利公式：A = P × (1 + r)^n
  // 简化为整数计算：A = P × (100 + r×100)^n / 100^n
  
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化计算：每年增长5%，三年增长约15.76%
  let year1 = multiply(principal, add(100, rate_percent)) / 100
  let year2 = multiply(year1, add(100, rate_percent)) / 100
  let year3 = multiply(year2, add(100, rate_percent)) / 100
  
  assert_eq(11576, year3)
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  // 计算排列数 P(n,k) = n × (n-1) × ... × (n-k+1)
  // P(5,3) = 5 × 4 × 3 = 60
  
  let n = 5
  let k = 3
  let permutation = multiply(n, multiply(n - 1, n - 2))
  assert_eq(60, permutation)
  
  // 计算组合数 C(n,k) = P(n,k) / k!
  // C(5,3) = 60 / (3 × 2 × 1) = 10
  let factorial_k = multiply(k, multiply(k - 1, k - 2))
  let combination = permutation / factorial_k
  assert_eq(10, combination)
}

test "physics_kinematics" {
  // 测试物理运动学计算
  // 位移公式：s = ut + (1/2)at²
  // 使用整数近似：s = ut + at²/2
  
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  let displacement_part1 = multiply(initial_velocity, time)
  let displacement_part2 = multiply(acceleration, multiply(time, time)) / 2
  let total_displacement = add(displacement_part1, displacement_part2)
  
  assert_eq(75, total_displacement)  // 10×5 + 2×5²/2 = 50 + 25 = 75
}

test "statistical_variance_calculation" {
  // 测试统计方差计算
  // 数据集：[10, 20, 30, 40, 50]
  // 平均值 = 30
  // 方差 = [(10-30)² + (20-30)² + (30-30)² + (40-30)² + (50-30)²] / 5
  
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  
  let diff1 = add(data1, -mean)
  let diff2 = add(data2, -mean)
  let diff3 = add(data3, -mean)
  let diff4 = add(data4, -mean)
  let diff5 = add(data5, -mean)
  
  let variance_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  let variance = variance_sum / 5
  
  assert_eq(200, variance)
}

test "cryptographic_simple_hash" {
  // 测试简单的哈希函数模拟
  // 使用多项式滚动哈希：hash = Σ(char_code × prime^i) mod modulus
  
  let input = "ABC"
  // 简化处理：A=65, B=66, C=67
  let char1 = 65
  let char2 = 66
  let char3 = 67
  let prime = 31
  let modulus = 1000000007
  
  // 计算哈希值（简化版，不取模）
  let hash_part1 = multiply(char1, multiply(prime, prime))
  let hash_part2 = multiply(char2, prime)
  let hash_part3 = char3
  let hash = add(add(hash_part1, hash_part2), hash_part3)
  
  assert_eq(65644, hash)  // 65×31² + 66×31 + 67 = 62465 + 2046 + 67 = 64578
}

test "game_logic_battle_simulation" {
  // 测试游戏战斗逻辑模拟
  let player_health = 100
  let player_attack = 20
  let enemy_health = 80
  let enemy_attack = 15
  
  // 第一回合
  let enemy_health_after_turn1 = add(enemy_health, -player_attack)
  let player_health_after_turn1 = add(player_health, -enemy_attack)
  
  // 第二回合
  let enemy_health_after_turn2 = add(enemy_health_after_turn1, -player_attack)
  let player_health_after_turn2 = add(player_health_after_turn1, -enemy_attack)
  
  // 第三回合
  let enemy_health_after_turn3 = add(enemy_health_after_turn2, -player_attack)
  let player_health_after_turn3 = add(player_health_after_turn2, -enemy_attack)
  
  // 验证战斗结果
  assert_true(enemy_health_after_turn3 <= 0)
  assert_true(player_health_after_turn3 > 0)
  
  // 具体验证
  assert_eq(20, enemy_health_after_turn3)   // 80 - 20×3 = 20
  assert_eq(55, player_health_after_turn3)  // 100 - 15×3 = 55
}