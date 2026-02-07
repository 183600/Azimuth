// Azimuth 项目增强测试套件
// 包含10个高质量的 MoonBit 测试用例，使用标准测试语法

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算复利：每年本金增加利率
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  // 验证计算结果
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "inventory_optimization" {
  // 库存优化算法测试
  let demand = 450
  let production_capacity = 100
  let safety_stock = 50
  
  // 计算生产批次数
  let batches_needed = divide_with_ceil(demand, production_capacity)
  
  // 计算总产量
  let total_production = multiply(batches_needed, production_capacity)
  
  // 计算剩余库存
  let remaining_inventory = subtract(total_production, demand)
  
  // 验证优化结果
  assert_eq(5, batches_needed)
  assert_eq(500, total_production)
  assert_eq(50, remaining_inventory)
}

test "temperature_conversion_system" {
  // 温度转换系统测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度（简化验证）
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  // 验证转换结果
  assert_eq(77, fahrenheit)
  assert_eq(25, celsius_back)
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 100
  let level = 5
  let multiplier = 3
  let bonus = 50
  let penalty = 20
  
  // 计算最终得分
  let level_score = multiply(base_score, level)
  let bonus_score = add(level_score, bonus)
  let final_score = subtract(bonus_score, penalty)
  
  // 计算星级评定
  let star_rating = divide_with_ceil(final_score, multiply(100, multiplier))
  
  // 验证评分系统
  assert_eq(500, level_score)
  assert_eq(550, bonus_score)
  assert_eq(530, final_score)
  assert_eq(2, star_rating)
}

test "data_compression_analysis" {
  // 数据压缩分析测试
  let original_size = 2000
  let compression_ratio = 4
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(original_size, compression_ratio)
  
  // 计算压缩率
  let compression_percentage = divide_with_ceil(multiply(subtract(original_size, compressed_size), 100), original_size)
  
  // 验证压缩效果
  assert_eq(500, compressed_size)
  assert_eq(75, compression_percentage)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_budget = 100000
  let team_size = 8
  let project_duration = 12
  
  // 计算人均预算
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  
  // 计算月度预算
  let monthly_budget = divide_with_ceil(total_budget, project_duration)
  
  // 计算人均月度预算
  let monthly_per_person = divide_with_ceil(budget_per_person, project_duration)
  
  // 验证资源分配
  assert_eq(12500, budget_per_person)
  assert_eq(8334, monthly_budget)
  assert_eq(1042, monthly_per_person)
}

test "binary_number_system" {
  // 二进制数字系统测试
  let decimal = 42
  
  // 分解为二进制位：42 = 32 + 8 + 2
  let bit_5 = multiply(1, 32)  // 2^5
  let bit_4 = multiply(0, 16)  // 2^4
  let bit_3 = multiply(1, 8)   // 2^3
  let bit_2 = multiply(0, 4)   // 2^2
  let bit_1 = multiply(1, 2)   // 2^1
  let bit_0 = multiply(0, 1)   // 2^0
  
  // 重新组合验证
  let reconstructed = add(add(add(add(add(bit_5, bit_4), bit_3), bit_2), bit_1), bit_0)
  
  // 计算设置位的数量
  let set_bits = add(add(1, 1), 1)
  
  // 验证二进制分解
  assert_eq(42, reconstructed)
  assert_eq(3, set_bits)
}

test "cryptographic_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key = 7
  let modulus = 1000
  
  // 简单加密过程
  let encrypted = multiply(message, key) % modulus
  
  // 解密过程（简化）
  let decrypted_step1 = divide_with_ceil(encrypted, key)
  
  // 验证加密过程
  assert_eq(415, encrypted)
  assert_eq(60, decrypted_step1)
}

test "physics_motion_calculation" {
  // 物理运动计算测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  
  // 计算最终速度：v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移：s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  // 验证物理计算
  assert_eq(20, final_velocity)
  assert_eq(75, displacement)
}

test "statistical_analysis" {
  // 统计分析测试
  let values = [12, 15, 18, 21, 24, 27, 30]
  
  // 计算总和
  let sum = add(add(add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4]), values[5]), values[6])
  
  // 计算平均值（向上取整）
  let count = 7
  let mean = divide_with_ceil(sum, count)
  
  // 计算中位数近似值
  let median = values[3]  // 已排序数组的中位数
  
  // 验证统计结果
  assert_eq(147, sum)
  assert_eq(21, mean)
  assert_eq(21, median)
}