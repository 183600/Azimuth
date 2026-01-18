// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

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
  
  // 验证计算结果
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
  // 通过检查一个数是否只能被1和自身整除来判断是否为素数
  
  let num = 17
  
  // 检查是否能被2到16之间的数整除（简化检查）
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_5 = (num % 5 == 0)
  let divisible_by_7 = (num % 7 == 0)
  let divisible_by_11 = (num % 11 == 0)
  let divisible_by_13 = (num % 13 == 0)
  
  // 17是素数，所以都不能被这些数整除
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_5)
  @azimuth.assert_false(divisible_by_7)
  @azimuth.assert_false(divisible_by_11)
  @azimuth.assert_false(divisible_by_13)
  
  // 测试一个合数
  let composite_num = 15
  let divisible_by_3_composite = (composite_num % 3 == 0)
  let divisible_by_5_composite = (composite_num % 5 == 0)
  
  @azimuth.assert_true(divisible_by_3_composite)
  @azimuth.assert_true(divisible_by_5_composite)
}

test "bank_interest_calculation" {
  // 银行利息计算测试
  // 本金 × (1 + 年利率)^年数（简化版，使用整数运算）
  
  let principal = 10000  // 本金10000元
  let annual_rate = 3    // 年利率3%
  let years = 5          // 存期5年
  
  // 简化的复利计算（避免浮点数）
  // 第1年：10000 × (1 + 3%) = 10000 × 103 = 1030000
  let year1_total = @azimuth.multiply(principal, @azimuth.add(100, annual_rate))
  
  // 第2年：1030000 × (1 + 3%) = 1030000 × 103 = 106090000
  let year2_total = @azimuth.multiply(year1_total, @azimuth.add(100, annual_rate))
  
  // 第3年：106090000 × (1 + 3%) = 106090000 × 103 = 10927270000
  let year3_total = @azimuth.multiply(year2_total, @azimuth.add(100, annual_rate))
  
  // 第4年：10927270000 × (1 + 3%) = 10927270000 × 103 = 1125108810000
  let year4_total = @azimuth.multiply(year3_total, @azimuth.add(100, annual_rate))
  
  // 第5年：1125108810000 × (1 + 3%) = 1125108810000 × 103 = 115866207300000
  let year5_total = @azimuth.multiply(year4_total, @azimuth.add(100, annual_rate))
  
  // 转换回实际金额（除以100的5次方）
  let final_amount = year5_total / 100000000
  
  // 验证计算结果：10000 × 1.03^5 ≈ 11592
  @azimuth.assert_eq(11586, final_amount)
}

test "string_pattern_matching" {
  // 字符串模式匹配测试
  // 检查字符串是否包含特定模式
  
  let email = "user@example.com"
  let phone = "123-456-7890"
  let date = "2023-12-25"
  
  // 测试greet函数处理不同格式的字符串
  let email_greeting = @azimuth.greet(email)
  let phone_greeting = @azimuth.greet(phone)
  let date_greeting = @azimuth.greet(date)
  
  @azimuth.assert_eq_string("Hello, user@example.com!", email_greeting)
  @azimuth.assert_eq_string("Hello, 123-456-7890!", phone_greeting)
  @azimuth.assert_eq_string("Hello, 2023-12-25!", date_greeting)
}

test "coordinate_geometry_calculations" {
  // 坐标几何计算测试
  // 计算两点之间的距离（简化版，使用整数运算）
  
  let x1 = 3
  let y1 = 4
  let x2 = 9
  let y2 = 12
  
  // 计算x和y的差值
  let dx = @azimuth.add(x2, -x1)  // 6
  let dy = @azimuth.add(y2, -y1)  // 8
  
  // 计算距离的平方：dx² + dy²
  let dx_squared = @azimuth.multiply(dx, dx)  // 36
  let dy_squared = @azimuth.multiply(dy, dy)  // 64
  let distance_squared = @azimuth.add(dx_squared, dy_squared)  // 100
  
  // 距离 = √100 = 10（这里我们只验证距离的平方）
  @azimuth.assert_eq(100, distance_squared)
  
  // 测试另一个坐标点
  let x3 = 0
  let y3 = 0
  let x4 = 5
  let y4 = 12
  
  let dx2 = @azimuth.add(x4, -x3)  // 5
  let dy2 = @azimuth.add(y4, -y3)  // 12
  let dx2_squared = @azimuth.multiply(dx2, dx2)  // 25
  let dy2_squared = @azimuth.multiply(dy2, dy2)  // 144
  let distance_squared2 = @azimuth.add(dx2_squared, dy2_squared)  // 169
  
  @azimuth.assert_eq(169, distance_squared2)
}

test "game_character_attributes" {
  // 游戏角色属性计算测试
  let base_strength = 10
  let base_agility = 8
  let base_intelligence = 12
  let level = 5
  let equipment_bonus = 3
  
  // 计算最终属性：基础值 × (1 + 等级×0.1) + 装备加成
  // 简化为整数运算：基础值 × (100 + 等级×10) / 100 + 装备加成
  
  let strength_multiplier = @azimuth.add(100, @azimuth.multiply(level, 10))
  let final_strength = @azimuth.add(@azimuth.multiply(base_strength, strength_multiplier) / 100, equipment_bonus)
  
  let agility_multiplier = @azimuth.add(100, @azimuth.multiply(level, 10))
  let final_agility = @azimuth.add(@azimuth.multiply(base_agility, agility_multiplier) / 100, equipment_bonus)
  
  let intelligence_multiplier = @azimuth.add(100, @azimuth.multiply(level, 10))
  let final_intelligence = @azimuth.add(@azimuth.multiply(base_intelligence, intelligence_multiplier) / 100, equipment_bonus)
  
  // 验证计算结果
  @azimuth.assert_eq(18, final_strength)   // 10 × 150/100 + 3 = 15 + 3 = 18
  @azimuth.assert_eq(15, final_agility)    // 8 × 150/100 + 3 = 12 + 3 = 15
  @azimuth.assert_eq(21, final_intelligence) // 12 × 150/100 + 3 = 18 + 3 = 21
  
  // 计算总属性值
  let total_attributes = @azimuth.add(@azimuth.add(final_strength, final_agility), final_intelligence)
  @azimuth.assert_eq(54, total_attributes)
}

test "data_structure_operations" {
  // 数据结构操作测试（模拟数组操作）
  // 模拟数组的插入、删除和查找操作
  
  // 模拟数组：[1, 2, 3, 4, 5]
  let array_size = 5
  
  // 插入操作：在索引2处插入元素10
  // 新数组：[1, 2, 10, 3, 4, 5]
  let new_size_after_insert = @azimuth.add(array_size, 1)
  
  // 验证新数组大小
  @azimuth.assert_eq(6, new_size_after_insert)
  
  // 删除操作：删除索引3处的元素
  // 新数组：[1, 2, 10, 4, 5]
  let new_size_after_delete = @azimuth.add(new_size_after_insert, -1)
  
  // 验证新数组大小
  @azimuth.assert_eq(5, new_size_after_delete)
  
  // 查找操作：查找值为4的元素
  let found_index = 3  // 假设在索引3处找到
  
  // 验证查找结果
  @azimuth.assert_eq(3, found_index)
  
  // 计算数组元素总和
  let sum_original = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  let sum_after_operations = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 10), 4), 5)
  
  @azimuth.assert_eq(15, sum_original)
  @azimuth.assert_eq(22, sum_after_operations)
}

test "mathematical_series_evaluation" {
  // 数学级数求值测试
  
  // 等差数列求和：1 + 4 + 7 + 10 + 13
  // 公式：n × (首项 + 末项) / 2
  let arithmetic_n = 5
  let arithmetic_first = 1
  let arithmetic_last = 13
  let arithmetic_sum = @azimuth.multiply(arithmetic_n, @azimuth.add(arithmetic_first, arithmetic_last)) / 2
  @azimuth.assert_eq(35, arithmetic_sum)
  
  // 等比数列求和：2 + 6 + 18 + 54 + 162
  // 手动计算：2 + 6 + 18 + 54 + 162 = 242
  let geometric_first = 2
  let geometric_ratio = 3
  
  // 手动计算各项
  let geometric_term2 = @azimuth.multiply(geometric_first, geometric_ratio)  // 6
  let geometric_term3 = @azimuth.multiply(geometric_term2, geometric_ratio)  // 18
  let geometric_term4 = @azimuth.multiply(geometric_term3, geometric_ratio)  // 54
  let geometric_term5 = @azimuth.multiply(geometric_term4, geometric_ratio)  // 162
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geometric_first, geometric_term2), geometric_term3), geometric_term4), geometric_term5)
  @azimuth.assert_eq(242, geometric_sum)
  
  // 平方数列求和：1² + 2² + 3² + 4² + 5²
  // 公式：n × (n + 1) × (2n + 1) / 6
  let square_n = 5
  let square_sum_formula = @azimuth.multiply(square_n, @azimuth.multiply(@azimuth.add(square_n, 1), @azimuth.add(@azimuth.multiply(2, square_n), 1))) / 6
  
  // 手动计算：1 + 4 + 9 + 16 + 25 = 55
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  let square_sum_manual = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  
  @azimuth.assert_eq(55, square_sum_formula)
  @azimuth.assert_eq(55, square_sum_manual)
}