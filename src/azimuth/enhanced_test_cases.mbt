// 增强测试用例 - 为 azimuth 库添加的高质量测试
// 包含实际应用场景、性能测试和复杂逻辑

test "scientific_calculation_precision" {
  // 科学计算精度测试
  // 计算物理公式：动能 = 0.5 * 质量 * 速度^2
  let mass = 10
  let velocity = 5
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(velocity, velocity)), 2)
  assert_eq(125, kinetic_energy)  // ceil(10 * 25 / 2) = ceil(125) = 125
  
  // 计势能：m * g * h
  let height = 20
  let gravity = 10  // 简化重力加速度
  let potential_energy = multiply(mass, multiply(gravity, height))
  assert_eq(2000, potential_energy)  // 10 * 10 * 20 = 2000
}

test "array_simulation_with_math" {
  // 使用数学函数模拟数组操作
  // 模拟数组求和：sum = Σ(i=1 to n) i
  let n = 10
  let sum_formula = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum_formula)  // ceil(10 * 11 / 2) = 55
  
  // 模拟数组平均值计算
  let total_sum = sum_formula
  let average = divide_with_ceil(total_sum, n)
  assert_eq(6, average)  // ceil(55/10) = 6
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 10000
  let annual_rate = 8  // 8%
  
  // 简化复利计算：每年计算一次并向上取整
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, annual_rate), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, annual_rate), 100))
  let amount_year4 = add(amount_year3, divide_with_ceil(multiply(amount_year3, annual_rate), 100))
  let amount_year5 = add(amount_year4, divide_with_ceil(multiply(amount_year4, annual_rate), 100))
  
  assert_eq(14702, amount_year5)  // 由于向上取整，结果会略高于标准复利
}

test "game_mechanics_calculation" {
  // 游戏机制计算：经验值和等级系统
  let base_exp = 100
  let level_multiplier = 15
  let current_level = 5
  
  // 计算升级所需经验：base_exp + (level * level_multiplier)
  let exp_needed = add(base_exp, multiply(current_level, level_multiplier))
  assert_eq(175, exp_needed)  // 100 + 5 * 15 = 175
  
  // 计算击败多个敌人获得的总经验
  let enemies_defeated = 8
  let exp_per_enemy = 25
  let total_exp_gained = multiply(enemies_defeated, exp_per_enemy)
  assert_eq(200, total_exp_gained)  // 8 * 25 = 200
  
  // 计算可以升级的次数
  let level_ups_possible = divide_with_ceil(total_exp_gained, exp_needed)
  assert_eq(2, level_ups_possible)  // ceil(200/175) = 2
}

test "logistics_optimization" {
  // 物流优化：集装箱装载计算
  let items_per_container = 48
  let total_orders = 235
  let containers_needed = divide_with_ceil(total_orders, items_per_container)
  
  assert_eq(5, containers_needed)  // ceil(235/48) = 5
  
  // 计算剩余空间
  let total_capacity = multiply(containers_needed, items_per_container)
  let unused_space = add(total_capacity, -total_orders)
  assert_eq(5, unused_space)  // 5*48 - 235 = 240 - 235 = 5
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_orders, 100), total_capacity)
  assert_eq(98, utilization_percent)  // ceil(235*100/240) = ceil(97.9) = 98
}

test "temperature_conversion_calculations" {
  // 温度转换计算（简化整数版本）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  assert_eq(77, fahrenheit)  // ceil(25*9/5) + 32 = ceil(45) + 32 = 77
  
  // 反向转换验证
  let back_to_celsius = divide_with_ceil(multiply(add(fahrenheit, -32), 5), 9)
  assert_eq(25, back_to_celsius)  // ceil((77-32)*5/9) = ceil(245/9) = ceil(27.2) = 28 向上取整
  
  // 多个温度点的批量转换
  let temps_celsius = 0
  let temps_fahrenheit = add(multiply(divide_with_ceil(multiply(temps_celsius, 9), 5), 1), 32)
  assert_eq(32, temps_fahrenheit)  // 0°C = 32°F
}

test "data_structure_simulation" {
  // 数据结构模拟：栈操作
  let stack_size = 10
  let operations_push = 7
  let operations_pop = 3
  let final_stack_size = add(stack_size, add(operations_push, -operations_pop))
  assert_eq(14, final_stack_size)  // 10 + 7 - 3 = 14
  
  // 模拟队列操作
  let queue_capacity = 20
  let enqueue_operations = 15
  let dequeue_operations = 8
  let current_queue_size = add(enqueue_operations, -dequeue_operations)
  let is_overflow = current_queue_size > queue_capacity
  assert_eq(7, current_queue_size)  // 15 - 8 = 7
  // 将布尔值转换为整数：false = 0, true = 1
  let overflow_int = if is_overflow { 1 } else { 0 }
  assert_eq(0, overflow_int)  // 7 <= 20，无溢出，所以 is_overflow = false = 0
}

test "cryptography_basic_operations" {
  // 基础密码学操作模拟（简化版）
  let message_value = 1234
  let key_value = 5678
  
  // 简单的加密操作（异或模拟）
  let encrypted = add(message_value, key_value)  // 简化为加法
  let decrypted = add(encrypted, -key_value)     // 简化为减法
  assert_eq(message_value, decrypted)  // 解密后应恢复原值
  
  // 密钥派生
  let base_key = 1000
  let iteration_count = 1000
  let derived_key = multiply(base_key, iteration_count)
  assert_eq(1000000, derived_key)  // 1000 * 1000 = 1000000
}

test "statistical_analysis_functions" {
  // 统计分析函数
  // 计算方差（简化版）
  let values_sum = 100
  let value_count = 10
  let mean = divide_with_ceil(values_sum, value_count)
  assert_eq(10, mean)  // ceil(100/10) = 10
  
  // 计算标准差（简化版）
  let squared_differences_sum = 50
  let variance = divide_with_ceil(squared_differences_sum, value_count)
  assert_eq(5, variance)  // ceil(50/10) = 5
  
  // 计算样本标准差
  let sample_variance = divide_with_ceil(squared_differences_sum, add(value_count, -1))
  assert_eq(6, sample_variance)  // ceil(50/9) = 6
}

test "geometry_calculations" {
  // 几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  assert_eq(96, area)  // 12 * 8 = 96
  assert_eq(40, perimeter)  // 2 * (12 + 8) = 40
  
  // 计算圆的面积和周长（简化整数版本）
  let radius = 7
  let pi_approx = 22  // 简化的π值
  let circle_area = divide_with_ceil(multiply(radius, multiply(radius, pi_approx)), 7)
  let circle_circumference = multiply(2, divide_with_ceil(multiply(radius, pi_approx), 7))
  assert_eq(154, circle_area)  // ceil(7*7*22/7) = 154
  assert_eq(44, circle_circumference)  // 2 * ceil(7*22/7) = 2 * 22 = 44
}