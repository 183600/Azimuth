// 高质量增强测试用例 - 为 Azimuth 项目添加
// 包含 10 个标准 MoonBit 测试用例，覆盖核心功能和边界情况

test "enhanced_arithmetic_precision" {
  // 增强算术精度测试
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(-10000, 10000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(-10000, -10000))
}

test "enhanced_divide_with_ceil_comprehensive" {
  // 增强向上取整除法综合测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "enhanced_string_processing_unicode" {
  // 增强字符串处理 Unicode 测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "enhanced_mathematical_properties" {
  // 增强数学性质测试
  let a = 24
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 5)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
  
  // 验证具体数值
  @azimuth.assert_eq(42, @azimuth.add(24, 18))
  @azimuth.assert_eq(432, @azimuth.multiply(24, 18))
}

test "enhanced_business_inventory_management" {
  // 增强业务库存管理测试
  let total_products = 258
  let box_capacity = 36
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(288, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(30, remaining_space)
  
  // 验证空间利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(90, utilization)
}

test "enhanced_complex_calculation_workflow" {
  // 增强复杂计算工作流测试
  // 场景：项目管理资源计算
  let team_members = 12
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_weeks = 3
  let work_days_per_week = 5
  let tasks_per_day = 6
  let sprint_capacity = @azimuth.multiply(sprint_weeks, @azimuth.multiply(work_days_per_week, tasks_per_day))
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(96, total_tasks)  // 12 * 8 = 96
  @azimuth.assert_eq(90, sprint_capacity)  // 3 * 5 * 6 = 90
  @azimuth.assert_eq(2, sprints_needed)  // ceil(96/90) = 2
}

test "enhanced_boundary_values_extreme" {
  // 增强边界值极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 边界值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_val))
}

test "enhanced_error_handling_robustness" {
  // 增强错误处理健壮性测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-9999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "enhanced_real_world_event_planning" {
  // 增强真实世界事件规划测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 10
  let cost_per_chair = 25
  let total_budget = 8000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(26, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(260, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(6500, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(1500, remaining_budget)
}

test "enhanced_financial_calculation_scenario" {
  // 增强金融计算场景测试
  let loan_amount = 150000
  let monthly_payment = 3500
  let interest_rate = 5  // 5% 利率
  
  // 计算需要的月数
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  @azimuth.assert_eq(43, months_needed)
  
  // 计算总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(150500, total_payment)
  
  // 计算总利息
  let total_interest = @azimuth.subtract(total_payment, loan_amount)
  @azimuth.assert_eq(500, total_interest)
  
  // 计算年数
  let years_needed = @azimuth.divide_with_ceil(months_needed, 12)
  @azimuth.assert_eq(4, years_needed)
  
  // 计算最后一年的月数
  let final_year_months = @azimuth.subtract(months_needed, @azimuth.multiply(years_needed - 1, 12))
  @azimuth.assert_eq(7, final_year_months)
}