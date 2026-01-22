// Azimuth 项目增强 MoonBit 测试用例集
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法
// 覆盖各种场景和边界情况

test "advanced_arithmetic_sequences" {
  // 测试高级算术序列计算
  // 计算斐波那契数列前几项的和：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  let fib9 = @azimuth.add(fib7, fib8)  // 34
  let fib10 = @azimuth.add(fib8, fib9) // 55
  
  // 计算前10项的和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8), fib9), fib10)
  
  @azimuth.assert_eq(143, sum)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User2023ProjectAlpha"
  let result2 = @azimuth.greet(alphanumeric)
  @azimuth.assert_eq_string("Hello, User2023ProjectAlpha!", result2)
  
  // 测试包含空格和各种语言字符的字符串
  let multilingual = "Hello 世界 123 Привет"
  let result3 = @azimuth.greet(multilingual)
  @azimuth.assert_eq_string("Hello, Hello 世界 123 Привет!", result3)
}

test "mathematical_series_and_progressions" {
  // 测试数学级数和数列
  // 计算等比数列：2, 4, 8, 16, 32, 64（前6项的和）
  let geo1 = 2
  let geo2 = @azimuth.multiply(geo1, 2)  // 4
  let geo3 = @azimuth.multiply(geo2, 2)  // 8
  let geo4 = @azimuth.multiply(geo3, 2)  // 16
  let geo5 = @azimuth.multiply(geo4, 2)  // 32
  let geo6 = @azimuth.multiply(geo5, 2)  // 64
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo1, geo2), geo3), geo4), geo5), geo6)
  @azimuth.assert_eq(126, geo_sum)
  
  // 计算平方数列：1, 4, 9, 16, 25（前5项的和）
  let sq1 = @azimuth.multiply(1, 1)
  let sq2 = @azimuth.multiply(2, 2)
  let sq3 = @azimuth.multiply(3, 3)
  let sq4 = @azimuth.multiply(4, 4)
  let sq5 = @azimuth.multiply(5, 5)
  
  let sq_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq1, sq2), sq3), sq4), sq5)
  @azimuth.assert_eq(55, sq_sum)
}

test "financial_calculations_advanced" {
  // 测试高级金融计算
  // 复利计算：本金 × (1 + 利率)^年数
  // 简化计算，避免浮点数
  let principal = 10000  // 本金
  let rate_percent = 8   // 8%年利率
  let years = 4          // 4年
  
  // 简化的复利计算：10000 × (1 + 8% × 4) = 10000 × 1.32 ≈ 13200
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor) / 100
  @azimuth.assert_eq(13200, total)
  
  // 贷款计算：月还款额 × 期数 - 本金 = 总利息
  let monthly_payment = 850
  let months = 24
  let loan_principal = 18000
  let total_paid = @azimuth.multiply(monthly_payment, months)
  let total_interest = @azimuth.add(total_paid, -loan_principal)
  @azimuth.assert_eq(2400, total_interest)
}

test "geometry_calculations_complex" {
  // 测试复杂几何计算
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 14
  let height = 6
  
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(66, trapezoid_area)
  
  // 圆柱体表面积（简化）：2 × π × r × (r + h) ≈ 2 × 3 × r × (r + h)
  let radius = 5
  let cylinder_height = 12
  let surface_area = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(radius, @azimuth.add(radius, cylinder_height)))
  @azimuth.assert_eq(510, surface_area)
  
  // 立方体对角线平方：a² + a² + a² = 3a²
  let cube_side = 7
  let diagonal_squared = @azimuth.multiply(3, @azimuth.multiply(cube_side, cube_side))
  @azimuth.assert_eq(147, diagonal_squared)
}

test "data_structure_simulation" {
  // 测试数据结构模拟
  // 模拟栈操作：push, push, pop, push
  let stack1 = 10
  let stack2 = 20
  let stack3 = @azimuth.add(stack1, stack2)  // 模拟push操作后的栈顶
  let popped = @azimuth.add(stack3, -stack2)  // 模拟pop操作，得到20
  let stack4 = @azimuth.add(popped, 30)  // 模拟push 30
  
  @azimuth.assert_eq(30, stack4)
  
  // 模拟队列操作：enqueue, enqueue, dequeue, enqueue
  let queue1 = 100
  let queue2 = 200
  let queue3 = @azimuth.add(queue1, queue2)  // 模拟队列状态
  let dequeued = @azimuth.add(queue3, -queue1)  // 模拟dequeue，得到200
  let queue4 = @azimuth.add(dequeued, 300)  // 模拟enqueue 300
  
  @azimuth.assert_eq(500, queue4)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟线性搜索：O(n)时间复杂度
  let array_size = 100
  let operations_per_element = 1
  let linear_operations = @azimuth.multiply(array_size, operations_per_element)
  @azimuth.assert_eq(100, linear_operations)
  
  // 模拟二分搜索：O(log n)时间复杂度（简化为除法）
  let binary_operations = array_size / 2
  @azimuth.assert_eq(50, binary_operations)
  
  // 模拟冒泡排序：O(n²)时间复杂度
  let bubble_operations = @azimuth.multiply(array_size, array_size)
  @azimuth.assert_eq(10000, bubble_operations)
  
  // 模拟快速排序平均情况：O(n log n)时间复杂度（简化）
  let quick_operations = @azimuth.multiply(array_size, array_size / 2)
  @azimuth.assert_eq(5000, quick_operations)
}

test "physics_calculations_simplified" {
  // 测试简化的物理计算
  // 自由落体距离：s = 0.5 × g × t²（简化为：s = 5 × t²）
  let time1 = 2
  let distance1 = @azimuth.multiply(5, @azimuth.multiply(time1, time1))
  @azimuth.assert_eq(20, distance1)
  
  let time2 = 3
  let distance2 = @azimuth.multiply(5, @azimuth.multiply(time2, time2))
  @azimuth.assert_eq(45, distance2)
  
  // 动能计算：E = 0.5 × m × v²（简化为：E = m × v² / 2）
  let mass = 10
  let velocity = 8
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(320, kinetic_energy)
  
  // 功的计算：W = F × d
  let force = 50
  let distance = 12
  let work = @azimuth.multiply(force, distance)
  @azimuth.assert_eq(600, work)
}

test "error_boundary_and_recovery" {
  // 测试错误边界和恢复
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护的边界情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试边界值的特殊运算
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试边界值的连续运算
  let chain1 = @azimuth.add(@azimuth.add(max_val, min_val), max_val)
  @azimuth.assert_eq(max_val, chain1)
  
  let chain2 = @azimuth.multiply(@azimuth.multiply(min_val, 1), -1)
  @azimuth.assert_eq(min_val, chain2)
  
  // 测试断言函数的边界情况
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(0, 1)(@azimuth.add(max_val, 1) > max_val)
  @azimuth.assert_eq(0, 1)(@azimuth.add(min_val, -1) < min_val)
}

test "real_world_integration_scenario" {
  // 测试真实世界集成场景：电子商务订单处理
  // 商品价格和数量
  let laptop_price = 5000
  let laptop_quantity = 1
  let mouse_price = 150
  let mouse_quantity = 2
  let keyboard_price = 300
  let keyboard_quantity = 1
  
  // 计算商品小计
  let laptop_subtotal = @azimuth.multiply(laptop_price, laptop_quantity)
  let mouse_subtotal = @azimuth.multiply(mouse_price, mouse_quantity)
  let keyboard_subtotal = @azimuth.multiply(keyboard_price, keyboard_quantity)
  
  // 计算订单总额
  let merchandise_total = @azimuth.add(@azimuth.add(laptop_subtotal, mouse_subtotal), keyboard_subtotal)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = @azimuth.multiply(merchandise_total, tax_rate) / 100
  
  // 计算运费（订单满5000免运费，否则80）
  let shipping_fee = if (merchandise_total >= 5000) { 0 } else { 80 }
  
  // 计算最终总额
  let final_total = @azimuth.add(@azimuth.add(merchandise_total, tax_amount), shipping_fee)
  
  // 验证计算结果：5000 + 150×2 + 300 = 5600
  // 税费：5600 × 8% = 448
  // 运费：0（满5000免运费）
  // 总额：5600 + 448 + 0 = 6048
  @azimuth.assert_eq(6048, final_total)
  
  // 测试会员折扣（VIP会员享受95折）
  let is_vip = true
  let discount_rate = if (is_vip) { 95 } else { 100 }
  let discounted_total = @azimuth.multiply(final_total, discount_rate) / 100
  @azimuth.assert_eq(5745, discounted_total)  // 6048 × 95% = 5745（四舍五入）
  
  // 测试积分奖励（每消费100元获得10积分）
  let points_earned = @azimuth.multiply(discounted_total, 10) / 100
  @azimuth.assert_eq(574, points_earned)  // 5745 ÷ 100 × 10 = 574（四舍五入）
}