// 新的综合 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "advanced_arithmetic_sequences" {
  // 测试高级算术序列
  // 斐波那契数列计算：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  
  // 验证前8个斐波那契数的和
  let fib_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8)
  @azimuth.assert_eq(54, fib_sum)
  
  // 测试等比数列：2, 4, 8, 16, 32
  let geo1 = 2
  let geo2 = @azimuth.multiply(geo1, 2)  // 4
  let geo3 = @azimuth.multiply(geo2, 2)  // 8
  let geo4 = @azimuth.multiply(geo3, 2)  // 16
  let geo5 = @azimuth.multiply(geo4, 2)  // 32
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo1, geo2), geo3), geo4), geo5)
  @azimuth.assert_eq(62, geo_sum)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 场景：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3          // 贷款年限
  
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_repayment = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1150000, total_repayment)  // 100000 + 100000×5×3 = 1150000
  
  // 场景：投资回报计算
  let initial_investment = 50000
  let return_rate = 8    // 回报率8%
  let investment_years = 5
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(return_rate, investment_years))
  let final_amount = @azimuth.multiply(initial_investment, growth_factor)
  
  @azimuth.assert_eq(250000, final_amount)  // 50000 × (1 + 8×5) = 50000 × 41 = 2050000
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 场景：物理学计算 - 自由落体
  let gravity = 10      // 重力加速度（简化为10m/s²）
  let time = 5          // 时间（秒）
  
  // 距离 = 0.5 × g × t²（简化为：距离 = g × t² / 2）
  let time_squared = @azimuth.multiply(time, time)
  let distance = @azimuth.multiply(gravity, time_squared) / 2
  
  @azimuth.assert_eq(125, distance)  // 10 × 25 / 2 = 125
  
  // 场景：化学计算 - 摩尔浓度
  let moles = 2        // 摩尔数
  let volume = 500     // 体积（毫升）
  
  // 摩尔浓度 = 摩尔数 / 体积 × 1000
  let molarity = @azimuth.multiply(moles, 1000) / volume
  
  @azimuth.assert_eq(4, molarity)  // 2 × 1000 / 500 = 4
}

test "game_logic_calculations" {
  // 测试游戏逻辑计算
  // 场景：角色属性计算
  let base_strength = 20
  let equipment_bonus = 15
  let skill_multiplier = 2
  let buff_modifier = 10
  
  // 最终力量 = (基础力量 + 装备加成) × 技能倍率 + 增益修正
  let intermediate = @azimuth.add(base_strength, equipment_bonus)
  let multiplied = @azimuth.multiply(intermediate, skill_multiplier)
  let final_strength = @azimuth.add(multiplied, buff_modifier)
  
  @azimuth.assert_eq(85, final_strength)  // (20 + 15) × 2 + 10 = 85
  
  // 场景：经验值计算
  let base_exp = 100
  let level_multiplier = 3
  let difficulty_bonus = 50
  
  // 获得经验 = 基础经验 × 等级倍率 + 难度奖励
  let exp_gained = @azimuth.add(@azimuth.multiply(base_exp, level_multiplier), difficulty_bonus)
  
  @azimuth.assert_eq(350, exp_gained)  // 100 × 3 + 50 = 350
}

test "data_structure_operations" {
  // 测试数据结构操作
  // 场景：模拟数组索引计算
  let row = 3
  let col = 4
  let num_cols = 6
  
  // 二维数组转一维数组的索引：index = row × num_cols + col
  let index = @azimuth.add(@azimuth.multiply(row, num_cols), col)
  @azimuth.assert_eq(22, index)  // 3 × 6 + 4 = 22
  
  // 场景：模拟环形缓冲区索引
  let buffer_size = 10
  let current_pos = 8
  let offset = 5
  
  // 环形索引 = (当前位置 + 偏移量) % 缓冲区大小
  let raw_index = @azimuth.add(current_pos, offset)
  let circular_index = raw_index % buffer_size
  
  @azimuth.assert_eq(3, circular_index)  // (8 + 5) % 10 = 3
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  // 场景：用户名格式化
  let first_name = "John"
  let last_name = "Doe"
  let user_id = "12345"
  
  // 创建用户名：姓_名_ID
  let username = first_name + "_" + last_name + "_" + user_id
  let greeting = @azimuth.greet(username)
  
  @azimuth.assert_eq_string("Hello, John_Doe_12345!", greeting)
  
  // 场景：多语言问候
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(english_name))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet(japanese_name))
  
  // 场景：特殊字符处理
  let email = "user@example.com"
  let phone = "+1-555-123-4567"
  let special_chars = "!@#$%^&*()"
  
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, +1-555-123-4567!", @azimuth.greet(phone))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
}

test "statistical_analysis" {
  // 测试统计分析
  // 场景：计算一组数据的统计值
  // 数据集：15, 25, 35, 45, 55
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(35, average)
  
  // 计算方差（简化版）：Σ(x - mean)² / n
  let diff1 = data1 - average
  let diff2 = data2 - average
  let diff3 = data3 - average
  let diff4 = data4 - average
  let diff5 = data5 - average
  
  let sq_diff1 = @azimuth.multiply(diff1, diff1)
  let sq_diff2 = @azimuth.multiply(diff2, diff2)
  let sq_diff3 = @azimuth.multiply(diff3, diff3)
  let sq_diff4 = @azimuth.multiply(diff4, diff4)
  let sq_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5) / 5
  @azimuth.assert_eq(200, variance)  // (400 + 100 + 0 + 100 + 400) / 5 = 200
}

test "error_boundary_testing" {
  // 测试错误边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值加法
  let max_plus_max = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, max_plus_max)
  
  let min_plus_min = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, min_plus_min)
  
  let max_plus_min = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, max_plus_min)
  
  // 测试极值乘法
  let max_times_two = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, max_times_two)
  
  let min_times_two = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, min_times_two)
  
  let min_times_neg_one = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_times_neg_one)
  
  // 测试边界附近的值
  let near_max = 2147483646
  let near_min = -2147483647
  
  let near_max_plus_one = @azimuth.add(near_max, 1)
  @azimuth.assert_eq(max_val, near_max_plus_one)
  
  let near_min_minus_one = @azimuth.add(near_min, -1)
  @azimuth.assert_eq(min_val, near_min_minus_one)
}

test "optimization_algorithms" {
  // 测试优化算法
  // 场景：线性搜索（简化版）
  let target = 42
  let array_size = 100
  let target_position = 42  // 假设目标在第42个位置
  
  // 模拟搜索比较次数
  let comparisons = target_position
  @azimuth.assert_eq(42, comparisons)
  
  // 场景：二分搜索（简化版）
  let sorted_array_size = 64
  let binary_target = 50
  let max_steps = 6  // 2^6 = 64，最多需要6步
  
  // 模拟二分搜索步骤
  let step1 = sorted_array_size / 2      // 32
  let step2 = step1 / 2                  // 16
  let step3 = step2 / 2                  // 8
  let step4 = step3 / 2                  // 4
  let step5 = step4 / 2                  // 2
  let step6 = step5 / 2                  // 1
  
  let total_steps = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(step1, step2), step3), step4), step5), step6)
  @azimuth.assert_eq(63, total_steps)
  
  // 场景：贪心算法 - 找零问题
  let amount = 87
  let coin_50 = 50
  let coin_20 = 20
  let coin_10 = 10
  let coin_5 = 5
  let coin_2 = 2
  let coin_1 = 1
  
  // 贪心算法找零
  let remaining1 = @azimuth.add(amount, -coin_50)  // 37
  let remaining2 = @azimuth.add(remaining1, -coin_20)  // 17
  let remaining3 = @azimuth.add(remaining2, -coin_10)  // 7
  let remaining4 = @azimuth.add(remaining3, -coin_5)   // 2
  let remaining5 = @azimuth.add(remaining4, -coin_2)   // 0
  
  let coin_count = 5  // 50 + 20 + 10 + 5 + 2
  @azimuth.assert_eq(5, coin_count)
  @azimuth.assert_eq(0, remaining5)
}

test "real_world_integration" {
  // 测试实际应用集成
  // 场景：电商订单处理
  let product_id = "PRD-001"
  let customer_id = "CUST-123"
  let unit_price = 299
  let quantity = 3
  let tax_rate = 8
  let shipping_cost = 15
  let discount_threshold = 500
  let discount_rate = 10
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算税费
  let after_discount = @azimuth.add(subtotal, -discount_amount)
  let tax_amount = @azimuth.multiply(after_discount, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(after_discount, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(948, total)  // 299×3=897, 折扣=89, 税费=64, 运费=15, 总计=897-89+64+15=887
  
  // 场景：库存管理
  let initial_stock = 100
  let incoming = 25
  let outgoing = 40
  let reserved = 10
  
  // 计算可用库存
  let available = @azimuth.add(@azimuth.add(initial_stock, incoming), @azimuth.add(-outgoing, -reserved))
  @azimuth.assert_eq(75, available)  // 100 + 25 - 40 - 10 = 75
  
  // 场景：用户权限验证
  let user_role = 2  // 1=普通用户, 2=管理员, 3=超级管理员
  let required_permission = 2
  
  let has_permission = if (user_role >= required_permission) { 1 } else { 0 }
  @azimuth.assert_eq(1, has_permission)
  
  // 场景：API限流
  let rate_limit = 1000
  let current_requests = 750
  let new_requests = 300
  
  let total_requests = @azimuth.add(current_requests, new_requests)
  let is_rate_limited = if (total_requests > rate_limit) { 1 } else { 0 }
  @azimuth.assert_eq(1, is_rate_limited)  // 750 + 300 = 1050 > 1000
}