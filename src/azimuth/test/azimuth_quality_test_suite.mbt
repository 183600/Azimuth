// Azimuth 项目高质量测试套件
// 使用标准 MoonBit 测试语法，包含实用场景测试

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(42, @azimuth.add(15, 27))
  @azimuth.assert_eq(405, @azimuth.multiply(15, 27))
}

test "shopping_cart_calculation" {
  // 测试购物车计算场景
  let apple_price = 8
  let apple_qty = 3
  let banana_price = 5
  let banana_qty = 4
  let shipping_fee = 12
  
  // 计算商品总价
  let apple_total = @azimuth.multiply(apple_price, apple_qty)
  let banana_total = @azimuth.multiply(banana_price, banana_qty)
  let subtotal = @azimuth.add(apple_total, banana_total)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(24, apple_total)
  @azimuth.assert_eq(20, banana_total)
  @azimuth.assert_eq(44, subtotal)
  @azimuth.assert_eq(56, total)
}

test "project_time_management" {
  // 测试项目时间管理场景
  let total_tasks = 47
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(days_needed, working_days_per_week)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(47/8) = 6
  @azimuth.assert_eq(2, weeks_needed)  // ceil(6/5) = 2
}

test "multilingual_greeting_system" {
  // 测试多语言问候系统
  let english_greeting = @azimuth.greet("World")
  let chinese_greeting = @azimuth.greet("世界")
  let spanish_greeting = @azimuth.greet("Mundo")
  let emoji_greeting = @azimuth.greet("🌍")
  let mixed_greeting = @azimuth.greet("User123")
  
  @azimuth.assert_eq_string("Hello, World!", english_greeting)
  @azimuth.assert_eq_string("Hello, 世界!", chinese_greeting)
  @azimuth.assert_eq_string("Hello, Mundo!", spanish_greeting)
  @azimuth.assert_eq_string("Hello, 🌍!", emoji_greeting)
  @azimuth.assert_eq_string("Hello, User123!", mixed_greeting)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_positive = 1000
  let min_negative = -1000
  let zero = 0
  
  // 测试极大值运算
  @azimuth.assert_eq(2000, @azimuth.add(max_positive, max_positive))
  @azimuth.assert_eq(1000000, @azimuth.multiply(max_positive, max_positive))
  
  // 测试极小值运算
  @azimuth.assert_eq(-2000, @azimuth.add(min_negative, min_negative))
  @azimuth.assert_eq(1000000, @azimuth.multiply(min_negative, min_negative))
  
  // 测试零值运算
  @azimuth.assert_eq(max_positive, @azimuth.add(max_positive, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(max_positive, zero))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_positive, zero))
}

test "compound_financial_calculation" {
  // 测试复合金融计算
  let principal = 10000
  let interest_rate = 8
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, interest_rate), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, interest_rate), 100))
  
  // 计算总利息
  let total_interest = @azimuth.add(year3_amount, -principal)
  
  @azimuth.assert_eq(10800, year1_amount)
  @azimuth.assert_eq(11664, year2_amount)
  @azimuth.assert_eq(12597, year3_amount)
  @azimuth.assert_eq(2597, total_interest)
}

test "error_handling_robustness" {
  // 测试错误处理健壮性
  // 测试除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  
  // 测试极小数值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积
  let floor_area = @azimuth.multiply(length, width)
  
  // 计算墙面总面积（四面墙）
  let wall_area = @azimuth.multiply(2, @azimuth.multiply(@azimuth.add(length, width), height))
  
  // 计算总表面积
  let total_surface_area = @azimuth.add(floor_area, wall_area)
  
  @azimuth.assert_eq(96, floor_area)
  @azimuth.assert_eq(200, wall_area)
  @azimuth.assert_eq(296, total_surface_area)
}

test "data_processing_pipeline" {
  // 测试数据处理管道
  let data_points = 150
  let batch_size = 25
  let processing_time_per_batch = 3
  let overhead_time = 10
  
  // 计算批次数
  let batches_needed = @azimuth.divide_with_ceil(data_points, batch_size)
  
  // 计算总处理时间
  let processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  let total_time = @azimuth.add(processing_time, overhead_time)
  
  @azimuth.assert_eq(6, batches_needed)  // ceil(150/25) = 6
  @azimuth.assert_eq(18, processing_time)  // 6 * 3 = 18
  @azimuth.assert_eq(28, total_time)  // 18 + 10 = 28
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 50000
  let team_a_cost = 15000
  let team_b_cost = 12000
  let team_c_cost = 18000
  
  // 计算已分配预算
  let allocated_budget = @azimuth.add(@azimuth.add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  
  // 计算可以支持的额外小项目数量（每个项目5000）
  let additional_projects = @azimuth.divide_with_ceil(remaining_budget, 5000)
  
  @azimuth.assert_eq(45000, allocated_budget)
  @azimuth.assert_eq(5000, remaining_budget)
  @azimuth.assert_eq(1, additional_projects)
}