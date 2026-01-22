// MoonBit 综合测试用例
// 覆盖 azimuth 项目的核心功能和实际应用场景

test "add_performance_with_large_numbers" {
  // 测试大数加法性能
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  
  @azimuth.assert_eq(2000000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-large_num1, -large_num2))
  @azimuth.assert_eq(0, @azimuth.add(large_num1, -large_num1))
}

test "multiply_with_decimal_simulation" {
  // 模拟小数乘法（使用整数表示）
  // 计算 12.5 * 8.3 ≈ 103.75，表示为 10375
  let num1 = 125  // 表示 12.5
  let num2 = 83   // 表示 8.3
  let result = @azimuth.multiply(num1, num2)
  
  // 结果应该是 10375，表示 103.75
  @azimuth.assert_eq(10375, result)
  
  // 测试负数小数
  let neg_num1 = -125
  let pos_num2 = 83
  let neg_result = @azimuth.multiply(neg_num1, pos_num2)
  @azimuth.assert_eq(-10375, neg_result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试 Unicode 表情符号
  let emoji = "🌟🚀💻"
  let emoji_result = @azimuth.greet(emoji)
  @azimuth.assert_eq_string("Hello, 🌟🚀💻!", emoji_result)
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let item_price = 299
  let quantity = 3
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping)
  
  @azimuth.assert_eq(1015, final_total)
}

test "scientific_calculation_simulation" {
  // 科学计算模拟：简单的物理公式
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10      // 质量
  let velocity = 15  // 速度
  
  // 计算 v^2
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  // 计算 1/2 * m * v^2，简化为 m * v^2 / 2
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2
  
  @azimuth.assert_eq(1125, kinetic_energy)
  
  // 计算势能：E = m * g * h
  let height = 20
  let gravity = 10  // 简化的重力加速度
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  
  @azimuth.assert_eq(2000, potential_energy)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  // 模拟数组索引计算：row * width + column
  let row = 3
  let column = 4
  let width = 10
  
  let index = @azimuth.add(@azimuth.multiply(row, width), column)
  @azimuth.assert_eq(34, index)
  
  // 模拟哈希表容量计算（下一个2的幂）
  let current_capacity = 17
  // 简化计算：找到大于等于 current_capacity * 2 的最小2的幂
  let target = @azimuth.multiply(current_capacity, 2)
  @azimuth.assert_eq(34, target)
}

test "game_mechanics_calculation" {
  // 游戏机制计算
  let base_damage = 100
  let level_multiplier = 3
  let critical_hit = 2
  let defense_reduction = 20
  
  // 计算基础伤害：base_damage * level_multiplier
  let damage_with_level = @azimuth.multiply(base_damage, level_multiplier)
  
  // 计算暴击伤害
  let critical_damage = @azimuth.multiply(damage_with_level, critical_hit)
  
  // 计算防御减免后的伤害
  let final_damage = @azimuth.add(critical_damage, -defense_reduction)
  
  @azimuth.assert_eq(580, final_damage)
  
  // 计算经验值奖励：基础经验 * (1 + 等级加成)
  let base_exp = 50
  let level_bonus = 5
  let total_exp = @azimuth.multiply(base_exp, @azimuth.add(1, level_bonus))
  
  @azimuth.assert_eq(300, total_exp)
}

test "statistical_analysis" {
  // 统计分析计算
  // 计算一组数据的平均值、方差等
  
  // 计算总和：10 + 20 + 30 + 40 + 50 = 150
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  
  // 计算平均值：150 / 5 = 30
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算标准差（简化）：平方根((10-30)² + (20-30)² + ... + (50-30)²) / 5
  // 这里简化为计算方差
  let diff1 = @azimuth.add(10, -30)
  let diff2 = @azimuth.add(20, -30)
  let diff3 = @azimuth.add(30, -30)
  let diff4 = @azimuth.add(40, -30)
  let diff5 = @azimuth.add(50, -30)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1),
    @azimuth.multiply(diff2, diff2)),
    @azimuth.multiply(diff3, diff3)),
    @azimuth.multiply(diff4, diff4)),
    @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(200, variance)
}

test "cryptography_simulation" {
  // 密码学模拟：简单的凯撒密码
  let message = 1234  // 表示要加密的数字
  let key = 7         // 密钥
  
  // 加密：(message + key) mod 10000（简化）
  let mut encrypted = @azimuth.add(message, key)
  if encrypted >= 10000 { encrypted = encrypted - 10000 }
  
  @azimuth.assert_eq(1241, encrypted)
  
  // 解密：(encrypted - key) mod 10000（简化）
  let mut decrypted = @azimuth.add(encrypted, -key)
  if decrypted < 0 { decrypted = decrypted + 10000 }
  
  @azimuth.assert_eq(1234, decrypted)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的边界处理
  let chain1 = @azimuth.add(@azimuth.add(max_val, 1), 1)
  @azimuth.assert_eq(max_val, chain1)
  
  let chain2 = @azimuth.multiply(@azimuth.multiply(min_val, 2), 2)
  @azimuth.assert_eq(min_val, chain2)
  
  // 测试混合运算的边界处理
  let mixed1 = @azimuth.add(@azimuth.multiply(max_val, 2), -100)
  @azimuth.assert_eq(max_val, mixed1)
  
  let mixed2 = @azimuth.multiply(@azimuth.add(min_val, 100), 2)
  @azimuth.assert_eq(min_val, mixed2)
  
  // 测试断言函数的边界情况
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(0, 1)(@azimuth.add(max_val, 1) > max_val)
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
}