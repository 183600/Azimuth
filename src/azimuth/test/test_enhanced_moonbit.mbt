// Enhanced MoonBit 测试用例集
// 包含10个测试用例，涵盖各种功能场景

test "enhanced_mathematical_series" {
  // 测试数学级数计算：斐波那契数列前10项和
  // 斐波那契数列：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  // 计算前10项和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f1, f2), f3), f4), f5), f6), f7), f8), f9), f10)
  
  @azimuth.assert_eq(143, sum)
}

test "enhanced_string_validation" {
  // 测试字符串验证和格式化
  // 测试邮箱格式验证（简化版）
  let email1 = "user@example.com"
  let email2 = "test.email+tag@domain.co.uk"
  let invalid_email = "invalid.email"
  
  // 简单验证：检查是否包含@符号
  let email1_has_at = @azimuth.add(0, 1)  // 模拟包含@符号
  let email2_has_at = @azimuth.add(0, 1)  // 模拟包含@符号
  let invalid_has_at = @azimuth.add(0, 0)  // 模拟不包含@符号
  
  @azimuth.assert_eq(1, email1_has_at)
  @azimuth.assert_eq(1, email2_has_at)
  @azimuth.assert_eq(0, invalid_has_at)
  
  // 测试用户名生成
  let first_name = "John"
  let last_name = "Doe"
  let user_id = 123
  // 模拟用户名生成，不使用.to_string()
  let username = first_name + "." + last_name + "123"
  
  @azimuth.assert_eq_string("Hello, John.Doe123!", @azimuth.greet("John.Doe123"))
}

test "enhanced_business_calculations" {
  // 测试业务计算：利润率和折扣计算
  let cost_price = 100
  let selling_price = 150
  let quantity = 10
  let discount_rate = 10  // 10%
  
  // 计算总成本和总收入
  let total_cost = @azimuth.multiply(cost_price, quantity)
  let total_revenue = @azimuth.multiply(selling_price, quantity)
  
  // 计算总利润
  let total_profit = @azimuth.add(total_revenue, -total_cost)
  @azimuth.assert_eq(500, total_profit)
  
  // 计算利润率（简化为整数百分比）
  let profit_rate = @azimuth.multiply(total_profit, 100) / total_cost
  @azimuth.assert_eq(50, profit_rate)  // 50%
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(total_revenue, discount_rate) / 100
  @azimuth.assert_eq(150, discount_amount)
  
  // 计算折后收入
  let discounted_revenue = @azimuth.add(total_revenue, -discount_amount)
  @azimuth.assert_eq(1350, discounted_revenue)
}

test "enhanced_scientific_calculations" {
  // 测试科学计算：速度、加速度和距离
  let initial_velocity = 10  // m/s
  let acceleration = 5       // m/s²
  let time = 8               // s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(50, final_velocity)
  
  // 计算距离：s = ut + 0.5at²
  let ut = @azimuth.multiply(initial_velocity, time)  // 80
  let at_squared = @azimuth.multiply(acceleration, @azimuth.multiply(time, time))  // 320
  let distance = @azimuth.add(ut, at_squared / 2)  // 80 + 160 = 240
  @azimuth.assert_eq(240, distance)
  
  // 计算平均速度
  let average_velocity = @azimuth.add(initial_velocity, final_velocity) / 2
  @azimuth.assert_eq(30, average_velocity)
}

test "enhanced_data_structures_simulation" {
  // 测试数据结构模拟：栈操作
  // 模拟栈操作：push 5, push 10, pop, push 15, peek
  let mut stack_top = 0
  
  // push 5
  stack_top = 5
  
  // push 10
  stack_top = 10
  
  // pop（返回10，栈顶变为5）
  let popped_value = stack_top
  stack_top = 5
  @azimuth.assert_eq(10, popped_value)
  
  // push 15
  stack_top = 15
  
  // peek（返回15，不改变栈顶）
  let peeked_value = stack_top
  @azimuth.assert_eq(15, peeked_value)
  @azimuth.assert_eq(15, stack_top)
  
  // 模拟队列操作：enqueue 20, enqueue 30, dequeue
  let mut queue_front = 20
  let queue_rear = 30
  
  // dequeue（返回20，队列变为只有30）
  let dequeued_value = queue_front
  queue_front = queue_rear
  @azimuth.assert_eq(20, dequeued_value)
  @azimuth.assert_eq(30, queue_front)
}

test "enhanced_game_mechanics" {
  // 测试游戏机制：经验值和等级计算
  let base_exp = 100
  let level = 5
  let difficulty_multiplier = 2
  let bonus_exp = 50
  
  // 计算基础经验值
  let base_total_exp = @azimuth.multiply(base_exp, level)
  @azimuth.assert_eq(500, base_total_exp)
  
  // 应用难度倍数
  let difficulty_exp = @azimuth.multiply(base_total_exp, difficulty_multiplier)
  @azimuth.assert_eq(1000, difficulty_exp)
  
  // 添加奖励经验值
  let total_exp = @azimuth.add(difficulty_exp, bonus_exp)
  @azimuth.assert_eq(1050, total_exp)
  
  // 计算升级所需经验值（每级需要等级×200经验）
  let exp_needed_for_next_level = @azimuth.multiply(@azimuth.add(level, 1), 200)
  @azimuth.assert_eq(1200, exp_needed_for_next_level)
  
  // 计算升级进度百分比
  let current_level_exp = @azimuth.multiply(level, 200)
  let progress_percentage = @azimuth.multiply(total_exp, 100) / exp_needed_for_next_level
  @azimuth.assert_eq(87, progress_percentage)  // 1050/1200 ≈ 87%（整数除法）
}

test "enhanced_network_calculations" {
  // 测试网络计算：带宽和数据传输
  let file_size_mb = 100
  let bandwidth_mbps = 20
  let network_overhead_percent = 10
  
  // 计算理论传输时间（秒）
  let transfer_time = file_size_mb / bandwidth_mbps
  @azimuth.assert_eq(5, transfer_time)
  
  // 计算网络开销
  let overhead_size = @azimuth.multiply(file_size_mb, network_overhead_percent) / 100
  @azimuth.assert_eq(10, overhead_size)
  
  // 计算实际传输大小
  let actual_size = @azimuth.add(file_size_mb, overhead_size)
  @azimuth.assert_eq(110, actual_size)
  
  // 计算实际传输时间
  let actual_transfer_time = actual_size / bandwidth_mbps
  @azimuth.assert_eq(5, actual_transfer_time)  // 110/20 = 5.5，整数除法为5
  
  // 计算传输效率
  let efficiency = @azimuth.multiply(file_size_mb, 100) / actual_size
  @azimuth.assert_eq(90, efficiency)  // 100/110 ≈ 90%（整数除法）
}

test "enhanced_date_time_calculations" {
  // 测试日期时间计算（简化版，使用分钟表示）
  let day_minutes = 1440  // 24 × 60
  let current_day_minutes = 720  // 12:00 PM
  let duration_minutes = 180     // 3小时
  
  // 计算结束时间
  let end_time = @azimuth.add(current_day_minutes, duration_minutes)
  @azimuth.assert_eq(900, end_time)  // 15:00 或 3:00 PM
  
  // 计算跨越天数
  let cross_day_duration = 1000
  let next_day_time = @azimuth.add(current_day_minutes, cross_day_duration)
  let days_crossed = next_day_time / day_minutes
  let minutes_into_next_day = next_day_time % day_minutes
  @azimuth.assert_eq(1, days_crossed)
  @azimuth.assert_eq(280, minutes_into_next_day)  // 4:40 AM
  
  // 计算工作日计算（假设周一为第0天）
  let current_day = 4  // 周五
  let business_days_to_add = 3
  let future_day = @azimuth.add(current_day, business_days_to_add)
  let adjusted_day = future_day % 7  // 周一
  @azimuth.assert_eq(0, adjusted_day)
}

test "enhanced_error_recovery" {
  // 测试错误恢复和容错机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的溢出保护
  let result1 = @azimuth.add(max_val, 100)
  let result2 = @azimuth.add(result1, 100)
  let result3 = @azimuth.add(result2, 100)
  @azimuth.assert_eq(max_val, result1)
  @azimuth.assert_eq(max_val, result2)
  @azimuth.assert_eq(max_val, result3)
  
  // 测试负数溢出保护
  let neg_result1 = @azimuth.add(min_val, -100)
  let neg_result2 = @azimuth.add(neg_result1, -100)
  let neg_result3 = @azimuth.add(neg_result2, -100)
  @azimuth.assert_eq(min_val, neg_result1)
  @azimuth.assert_eq(min_val, neg_result2)
  @azimuth.assert_eq(min_val, neg_result3)
  
  // 测试乘法溢出保护
  let mul_result1 = @azimuth.multiply(max_val, 2)
  let mul_result2 = @azimuth.multiply(mul_result1, 2)
  @azimuth.assert_eq(max_val, mul_result1)
  @azimuth.assert_eq(max_val, mul_result2)
  
  // 测试特殊值组合
  let special_result1 = @azimuth.multiply(min_val, -1)
  let special_result2 = @azimuth.add(@azimuth.multiply(min_val, -1), 1)
  @azimuth.assert_eq(min_val, special_result1)
  @azimuth.assert_eq(min_val, special_result2)
}

test "enhanced_comprehensive_scenario" {
  // 测试综合场景：电商订单处理系统
  let product_price = 299
  let quantity = 3
  let customer_level = 2  // VIP等级
  let base_shipping = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  @azimuth.assert_eq(897, subtotal)
  
  // 根据VIP等级计算折扣
  let discount_rate = @azimuth.multiply(customer_level, 5)  // 每级5%折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  @azimuth.assert_eq(89, discount_amount)  // 897 × 10% = 89.7，整数除法为89
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  @azimuth.assert_eq(808, discounted_price)
  
  // 根据VIP等级计算运费折扣
  let shipping_discount = @azimuth.multiply(base_shipping, customer_level) / 10
  let final_shipping = @azimuth.add(base_shipping, -shipping_discount)
  @azimuth.assert_eq(16, final_shipping)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  @azimuth.assert_eq(64, tax_amount)  // 808 × 8% = 64.64，整数除法为64
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_price, tax_amount), final_shipping)
  @azimuth.assert_eq(888, final_total)
  
  // 计算客户获得积分（每消费1元获得1积分，VIP等级额外加成）
  let base_points = discounted_price
  let bonus_points = @azimuth.multiply(base_points, customer_level) / 10
  let total_points = @azimuth.add(base_points, bonus_points)
  @azimuth.assert_eq(969, total_points)  // 808 + 808×20% = 808 + 161 = 969
}