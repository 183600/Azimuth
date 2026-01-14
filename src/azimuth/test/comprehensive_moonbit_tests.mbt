// 综合MoonBit测试用例
// 测试azimuth模块的各种功能和边界情况

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
}

test "prime_number_validation" {
  // 素数验证测试（简化版）
  // 检查一个数是否为素数（通过测试是否能被小于它的数整除）
  
  let is_prime_7 = {
    // 7不能被2,3,4,5,6整除
    let not_div_by_2 = (7 % 2 != 0)
    let not_div_by_3 = (7 % 3 != 0)
    let not_div_by_4 = (7 % 4 != 0)
    let not_div_by_5 = (7 % 5 != 0)
    let not_div_by_6 = (7 % 6 != 0)
    not_div_by_2 && not_div_by_3 && not_div_by_4 && not_div_by_5 && not_div_by_6
  }
  
  let is_prime_9 = {
    // 9能被3整除
    (9 % 3 != 0)
  }
  
  @azimuth.assert_true(is_prime_7)
  @azimuth.assert_false(is_prime_9)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试包含各种特殊字符的字符串
  
  let unicode_string = "Hello, 世界! 🌍"
  let result1 = @azimuth.greet(unicode_string)
  @azimuth.assert_eq_string("Hello, Hello, 世界! 🌍!", result1)
  
  // 测试包含转义字符的字符串
  let escaped_string = "Line1\nLine2\tTabbed"
  let result2 = @azimuth.greet(escaped_string)
  @azimuth.assert_eq_string("Hello, Line1\nLine2\tTabbed!", result2)
  
  // 测试包含引号的字符串
  let quoted_string = "\"Quote\" inside"
  let result3 = @azimuth.greet(quoted_string)
  @azimuth.assert_eq_string("Hello, \"Quote\" inside!", result3)
}

test "matrix_multiplication_simulation" {
  // 模拟2x2矩阵乘法
  // 矩阵A: [a b]  矩阵B: [e f]
  //         [c d]          [g h]
  // 结果: [a*e+b*g, a*f+b*h]
  //       [c*e+d*g, c*f+d*h]
  
  let a = 1; let b = 2
  let c = 3; let d = 4
  let e = 5; let f = 6
  let g = 7; let h = 8
  
  // 计算结果矩阵的元素
  let result_00 = @azimuth.add(@azimuth.multiply(a, e), @azimuth.multiply(b, g))  // 1*5 + 2*7 = 19
  let result_01 = @azimuth.add(@azimuth.multiply(a, f), @azimuth.multiply(b, h))  // 1*6 + 2*8 = 22
  let result_10 = @azimuth.add(@azimuth.multiply(c, e), @azimuth.multiply(d, g))  // 3*5 + 4*7 = 43
  let result_11 = @azimuth.add(@azimuth.multiply(c, f), @azimuth.multiply(d, h))  // 3*6 + 4*8 = 50
  
  @azimuth.assert_eq(19, result_00)
  @azimuth.assert_eq(22, result_01)
  @azimuth.assert_eq(43, result_10)
  @azimuth.assert_eq(50, result_11)
}

test "physics_simulation" {
  // 物理模拟测试：计算自由落体运动
  // 公式：h = h0 - 0.5 * g * t^2
  // 简化为整数计算：h = h0 - 5 * t^2 (g ≈ 10)
  
  let initial_height = 100  // 初始高度100米
  let g_half = 5  // 0.5 * g 的近似值
  
  // 计算不同时间的高度
  let height_t1 = @azimuth.add(initial_height, @azimuth.multiply(-g_half, @azimuth.multiply(1, 1)))  // 100 - 5*1 = 95
  let height_t2 = @azimuth.add(initial_height, @azimuth.multiply(-g_half, @azimuth.multiply(2, 2)))  // 100 - 5*4 = 80
  let height_t3 = @azimuth.add(initial_height, @azimuth.multiply(-g_half, @azimuth.multiply(3, 3)))  // 100 - 5*9 = 55
  let height_t4 = @azimuth.add(initial_height, @azimuth.multiply(-g_half, @azimuth.multiply(4, 4)))  // 100 - 5*16 = 20
  
  @azimuth.assert_eq(95, height_t1)
  @azimuth.assert_eq(80, height_t2)
  @azimuth.assert_eq(55, height_t3)
  @azimuth.assert_eq(20, height_t4)
}

test "statistical_analysis" {
  // 统计分析测试：计算方差和标准差（简化版）
  // 数据集：[10, 20, 30, 40, 50]
  
  // 计算平均值
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)  // 150
  let mean = data_sum / 5  // 30
  
  // 计算方差：每个数据点与平均值差的平方和的平均值
  let diff1 = @azimuth.add(10, -mean)  // -20
  let diff2 = @azimuth.add(20, -mean)  // -10
  let diff3 = @azimuth.add(30, -mean)  // 0
  let diff4 = @azimuth.add(40, -mean)  // 10
  let diff5 = @azimuth.add(50, -mean)  // 20
  
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1),  // 400
    @azimuth.multiply(diff2, diff2)), // 100
    @azimuth.multiply(diff3, diff3)), // 0
    @azimuth.multiply(diff4, diff4)), // 100
    @azimuth.multiply(diff5, diff5))  // 400
  
  let variance = square_sum / 5  // 200
  
  @azimuth.assert_eq(30, mean)
  @azimuth.assert_eq(200, variance)
}

test "cryptography_simulation" {
  // 密码学模拟测试：简化的凯撒密码
  // 将字母向后移动3位
  
  // 模拟字母表位置（A=0, B=1, ..., Z=25）
  let letter_A = 0
  let letter_D = 3
  let letter_X = 23
  let letter_A_shifted = @azimuth.add(letter_X, 3)  // 26，应该循环到0
  let letter_A_wrapped = letter_A_shifted % 26  // 0
  
  @azimuth.assert_eq(0, letter_A_wrapped)
  
  // 测试解密：向后移动3位等于向前移动23位
  let letter_original = @azimuth.add(letter_D, 23)  // 26
  let letter_wrapped = letter_original % 26  // 0
  
  @azimuth.assert_eq(0, letter_wrapped)
}

test "game_logic_simulation" {
  // 游戏逻辑模拟测试：简单的RPG战斗计算
  
  let player_hp = 100
  let player_attack = 25
  let player_defense = 10
  
  let enemy_hp = 80
  let enemy_attack = 20
  let enemy_defense = 5
  
  // 计算实际伤害：攻击力 - 防御力
  let player_damage = @azimuth.add(player_attack, -enemy_defense)  // 20
  let enemy_damage = @azimuth.add(enemy_attack, -player_defense)   // 10
  
  // 第一轮战斗
  let enemy_hp_after_round1 = @azimuth.add(enemy_hp, -player_damage)  // 60
  let player_hp_after_round1 = @azimuth.add(player_hp, -enemy_damage) // 90
  
  // 第二轮战斗
  let enemy_hp_after_round2 = @azimuth.add(enemy_hp_after_round1, -player_damage)  // 40
  let player_hp_after_round2 = @azimuth.add(player_hp_after_round1, -enemy_damage) // 80
  
  // 第三轮战斗
  let enemy_hp_after_round3 = @azimuth.add(enemy_hp_after_round2, -player_damage)  // 20
  let player_hp_after_round3 = @azimuth.add(player_hp_after_round2, -enemy_damage) // 70
  
  // 第四轮战斗
  let enemy_hp_after_round4 = @azimuth.add(enemy_hp_after_round3, -player_damage)  // 0
  let player_hp_after_round4 = @azimuth.add(player_hp_after_round3, -enemy_damage) // 60
  
  @azimuth.assert_eq(0, enemy_hp_after_round4)  // 敌人被击败
  @azimuth.assert_eq(60, player_hp_after_round4)  // 玩家剩余HP
}

test "financial_compound_interest" {
  // 金融复利计算测试
  // 公式：A = P(1 + r/n)^(nt)
  // 简化为整数计算：A = P(1 + r*t)  （近似计算）
  
  let principal = 10000  // 本金
  let rate_percent = 5   // 年利率5%
  let years = 3          // 3年
  
  // 简化的复利计算
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))  // 115
  let total_amount = @azimuth.multiply(principal, growth_factor) / 100  // 11500
  
  @azimuth.assert_eq(11500, total_amount)
  
  // 计算利息
  let interest = @azimuth.add(total_amount, -principal)
  @azimuth.assert_eq(1500, interest)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试：模拟不同时间复杂度的算法
  
  let n = 10
  
  // O(1) 常数时间
  let constant_time = 1
  
  // O(n) 线性时间
  let linear_time = n
  
  // O(n^2) 平方时间
  let quadratic_time = @azimuth.multiply(n, n)
  
  // O(n^3) 立方时间
  let cubic_time = @azimuth.multiply(@azimuth.multiply(n, n), n)
  
  // O(2^n) 指数时间（简化为2的n次方）
  let exponential_time = {
    // 手动计算2^10 = 1024
    @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(
      @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)),
      @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))), 2), 2)
  }
  
  @azimuth.assert_eq(1, constant_time)
  @azimuth.assert_eq(10, linear_time)
  @azimuth.assert_eq(100, quadratic_time)
  @azimuth.assert_eq(1000, cubic_time)
  @azimuth.assert_eq(1024, exponential_time)
}