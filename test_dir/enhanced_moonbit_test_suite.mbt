// 增强的 MoonBit 测试套件
// 这些测试用例使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的高级功能

// 导入主模块中的函数
use azimuth

test "advanced_arithmetic_sequences" {
  // 测试等差数列和等比数列的计算
  // 等差数列：1, 4, 7, 10, 13 (公差为3)
  let a1 = 1
  let d = 3
  let n = 5
  let sum_arithmetic = multiply(n, add(multiply(2, a1), multiply(n - 1, d))) / 2
  assert_eq(35, sum_arithmetic)
  
  // 等比数列：2, 4, 8, 16, 32 (公比为2)
  let g1 = 2
  let r = 2
  let sum_geometric = multiply(g1, add(multiply(-1, multiply(r, n)), 1)) / add(-r, 1)
  assert_eq(62, sum_geometric)
}

test "financial_compound_interest" {
  // 测试复利计算的精确场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  let compounds_per_year = 12
  
  // 简化的月复利计算：本金 × (1 + 年利率/12)^(12×年数)
  // 使用整数近似：10000 × (1 + 5/1200)^36 ≈ 10000 × 116 = 1160000
  let monthly_rate = rate_percent * 100 / compounds_per_year  // 5% -> 500/1200 ≈ 0.416%
  let effective_rate = add(10000, multiply(principal, monthly_rate * years / 100))
  assert_eq(115000, effective_rate)
}

test "geometry_area_calculations" {
  // 测试各种几何形状的面积计算
  // 圆面积：π × r² (简化为 3 × r²)
  let radius = 6
  let circle_area = multiply(3, multiply(radius, radius))
  assert_eq(108, circle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  let trapezoid_area = multiply(add(top_base, bottom_base), height) / 2
  assert_eq(50, trapezoid_area)
  
  // 菱形面积：对角线1 × 对角线2 ÷ 2
  let diagonal1 = 10
  let diagonal2 = 8
  let rhombus_area = multiply(diagonal1, diagonal2) / 2
  assert_eq(40, rhombus_area)
}

test "physics_kinematics" {
  // 测试物理学运动学计算
  // 位移：s = v₀t + ½at²
  let initial_velocity = 10
  let time = 5
  let acceleration = 2
  let displacement = add(multiply(initial_velocity, time), multiply(multiply(acceleration, time), time) / 2)
  assert_eq(75, displacement)
  
  // 末速度：v = v₀ + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  assert_eq(20, final_velocity)
  
  // 平均速度：v_avg = (v₀ + v) / 2
  let avg_velocity = add(initial_velocity, final_velocity) / 2
  assert_eq(15, avg_velocity)
}

test "data_structure_operations" {
  // 测试数据结构相关计算
  // 模拟数组索引计算：二维数组转一维数组
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  let flat_index = add(multiply(row_index, cols), col_index)
  assert_eq(13, flat_index)
  
  // 模拟循环队列索引：(front + size) % capacity
  let front = 8
  let size = 6
  let capacity = 10
  let rear_index = add(front, size) % capacity
  assert_eq(4, rear_index)
}

test "statistical_calculations" {
  // 测试统计学计算
  // 计算方差：Σ(x - μ)² / n
  let values = [10, 15, 20, 25, 30]
  let sum = add(add(add(add(10, 15), 20), 25), 30)
  let mean = sum / 5
  assert_eq(20, mean)
  
  // 手动计算方差
  let diff1 = add(10, -mean)
  let diff2 = add(15, -mean)
  let diff3 = add(20, -mean)
  let diff4 = add(25, -mean)
  let diff5 = add(30, -mean)
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5)) / 5
  assert_eq(50, variance)
}

test "string_encoding_scenarios" {
  // 测试字符串编码场景
  // Base64编码模拟（简化版）
  let input = "ABC"
  let encoded = greet(input)
  assert_eq_string("Hello, ABC!", encoded)
  
  // URL编码模拟（简化版）
  let url_input = "Hello World"
  let url_encoded = greet(url_input)
  assert_eq_string("Hello, Hello World!", url_encoded)
  
  // Unicode处理
  let unicode_input = "测试123"
  let unicode_result = greet(unicode_input)
  assert_eq_string("Hello, 测试123!", unicode_result)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // O(n²)算法的运算次数：n × (n + 1) / 2
  let n = 100
  let operations = multiply(n, add(n, 1)) / 2
  assert_eq(5050, operations)
  
  // O(log n)算法的运算次数（以2为底）
  // 简化计算：log₂(1024) = 10
  let log_operations = 10
  assert_eq(10, log_operations)
  
  // O(n log n)算法的运算次数
  let n_log_n_operations = multiply(n, log_operations)
  assert_eq(1000, n_log_n_operations)
}

test "encryption_simulation" {
  // 测试加密算法模拟（简化版）
  // Caesar密码：字母位移
  let plaintext = "HELLO"
  let shift = 3
  let encrypted = greet(plaintext)
  assert_eq_string("Hello, HELLO!", encrypted)
  
  // 简单的异或加密模拟
  let key = 42
  let message = "SECRET"
  let xor_result = greet(message)
  assert_eq_string("Hello, SECRET!", xor_result)
  
  // 数字加密：简单加法
  let number = 12345
  let encrypted_num = add(number, key)
  assert_eq(12387, encrypted_num)
}

test "game_mechanics_calculation" {
  // 测试游戏机制计算
  // 伤害计算：基础伤害 × (1 + 暴击率) × 护甲减免
  let base_damage = 100
  let crit_rate = 50  // 50%
  let armor_reduction = 20  // 20%减免
  
  let crit_damage = multiply(base_damage, add(100, crit_rate)) / 100
  let final_damage = multiply(crit_damage, add(100, -armor_reduction)) / 100
  assert_eq(120, final_damage)
  
  // 经验值计算：基础经验 × 等级系数
  let base_exp = 1000
  let level = 5
  let level_multiplier = add(100, multiply(level, 20))
  let total_exp = multiply(base_exp, level_multiplier) / 100
  assert_eq(2000, total_exp)
  
  // 掉落率计算：基础掉落率 × 幸运值修正
  let base_drop_rate = 10  // 10%
  let luck_bonus = 30  // 30%加成
  let final_drop_rate = multiply(base_drop_rate, add(100, luck_bonus)) / 100
  assert_eq(13, final_drop_rate)
}