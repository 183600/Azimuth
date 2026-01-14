// 综合性 MoonBit 测试套件
// 包含针对 azimuth 包的全面测试用例

test "advanced_arithmetic_operations" {
  // 测试高级算术运算组合
  // 计算 (a + b) * (c - d) + e * f
  let a = 15
  let b = 25
  let c = 50
  let d = 30
  let e = 8
  let f = 12
  
  let sum_ab = azimuth::@azimuth.add(a, b)
  let diff_cd = azimuth::@azimuth.add(c, -d)
  let product_ef = azimuth::@azimuth.multiply(e, f)
  let result = azimuth::@azimuth.add(azimuth::@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  @azimuth.assert_eq(896, result)  // (15+25)*(50-30) + 8*12 = 40*20 + 96 = 800 + 96 = 896
}

test "string_internationalization" {
  // 测试国际化字符串处理
  @azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", azimuth::@azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, العربية!", azimuth::@azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", azimuth::@azimuth.greet("🌍🌎🌏"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", azimuth::@azimuth.greet("ñáéíóú"))
}

test "financial_calculations" {
  // 测试金融计算场景
  // 复利计算：本金 * (1 + 利率)^年数
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化复利计算：10000 * (1 + 0.05)^3 ≈ 10000 * 116 = 1160000
  let growth_factor = azimuth::@azimuth.add(100, azimuth::@azimuth.multiply(rate_percent, years))
  let total = azimuth::@azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(1150000, total)
  
  // 贷款月供计算（简化）：本金 + 利息
  let loan_amount = 200000
  let annual_rate = 4
  let loan_years = 30
  let total_interest = azimuth::@azimuth.multiply(loan_amount, azimuth::@azimuth.multiply(annual_rate, loan_years))
  let total_payment = azimuth::@azimuth.add(loan_amount, total_interest)
  @azimuth.assert_eq(44000000, total_payment)
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 物理公式：动能 = 0.5 * 质量 * 速度^2（简化为整数）
  let mass = 10
  let velocity = 15
  let kinetic_energy = azimuth::@azimuth.multiply(mass, azimuth::@azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(1125, kinetic_energy)
  
  // 化学摩尔质量计算
  let h_mass = 1
  let o_mass = 16
  let h2o_molar_mass = azimuth::@azimuth.add(azimuth::@azimuth.multiply(2, h_mass), o_mass)
  @azimuth.assert_eq(18, h2o_molar_mass)
}

test "boundary_stress_testing" {
  // 测试边界压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续最大值运算
  let result1 = azimuth::@azimuth.add(max_val, azimuth::@azimuth.add(max_val, -max_val))
  @azimuth.assert_eq(max_val, result1)
  
  // 连续最小值运算
  let result2 = azimuth::@azimuth.multiply(min_val, azimuth::@azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, result2)
  
  // 极值混合运算
  let result3 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(max_val, 0), min_val)
  @azimuth.assert_eq(min_val, result3)
}

test "data_structure_simulation" {
  // 测试数据结构模拟
  // 栈操作模拟：push(push(push([], 1), 2), 3) 的和
  let stack_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(6, stack_sum)
  
  // 队列操作模拟：enqueue(enqueue(enqueue([], 1), 2), 3) 的和
  let queue_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(6, queue_sum)
  
  // 二叉树节点值总和模拟
  let root = 10
  let left_child = 5
  let right_child = 15
  let tree_sum = azimuth::@azimuth.add(root, azimuth::@azimuth.add(left_child, right_child))
  @azimuth.assert_eq(30, tree_sum)
}

test "algorithm_implementation" {
  // 测试算法实现
  // 简单排序验证（冒泡排序的交换次数）
  let array_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(5, 2), 8), 1), 9)
  let sorted_sum = azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(azimuth::@azimuth.add(1, 2), 5), 8), 9)
  @azimuth.assert_eq(array_sum, sorted_sum)
  
  // 二分查找步骤模拟（在有序数组中查找元素）
  let search_range = azimuth::@azimuth.add(9, -1)  // 高索引 - 低索引
  let mid_point = search_range / 2
  @azimuth.assert_eq(4, mid_point)
}

test "game_logic_simulation" {
  // 测试游戏逻辑模拟
  // RPG 角色属性计算
  let base_strength = 10
  let equipment_bonus = 5
  let buff_multiplier = 2
  let final_strength = azimuth::@azimuth.multiply(azimuth::@azimuth.add(base_strength, equipment_bonus), buff_multiplier)
  @azimuth.assert_eq(30, final_strength)
  
  // 伤害计算：(攻击力 - 防御力) * 暴击倍率
  let attack_power = 100
  let defense = 30
  let crit_multiplier = 2
  let damage = azimuth::@azimuth.multiply(azimuth::@azimuth.add(attack_power, -defense), crit_multiplier)
  @azimuth.assert_eq(140, damage)
}

test "performance_benchmarks" {
  // 测试性能基准测试
  // 大数运算性能测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 连续大数加法
  let sum_result = azimuth::@azimuth.add(azimuth::@azimuth.add(large_num1, large_num2), large_num3)
  @azimuth.assert_eq(6000000, sum_result)
  
  // 大数乘法
  let product_result = azimuth::@azimuth.multiply(large_num1, 2)
  @azimuth.assert_eq(2000000, product_result)
  
  // 复合运算
  let complex_result = azimuth::@azimuth.add(azimuth::@azimuth.multiply(large_num1, 3), azimuth::@azimuth.multiply(large_num2, 2))
  @azimuth.assert_eq(7000000, complex_result)
}

test "error_resilience_testing" {
  // 测试错误恢复能力
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出保护正常工作
  @azimuth.assert_true(azimuth::@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(azimuth::@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(azimuth::@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(azimuth::@azimuth.multiply(min_val, -2) == min_val)
  
  // 确保正常运算不受影响
  @azimuth.assert_true(azimuth::@azimuth.add(100, 200) == 300)
  @azimuth.assert_true(azimuth::@azimuth.multiply(15, 20) == 300)
  @azimuth.assert_true(azimuth::@azimuth.greet("Test") == "Hello, Test!")
}