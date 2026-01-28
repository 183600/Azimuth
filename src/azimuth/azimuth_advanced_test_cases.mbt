// 新增的标准 MoonBit 测试用例 - 针对 azimuth 库的高级功能测试
// 使用标准的 MoonBit 测试语法，专注于数学性质和复杂业务场景

test "mathematical_commutative_laws" {
  // 测试加法和乘法交换律
  let a = 17
  let b = 29
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_associative_laws" {
  // 测试加法和乘法结合律
  let a = 5
  let b = 8
  let c = 12
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "mathematical_distributive_law" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期付款计算
  let loan_amount = 150000
  let annual_interest_rate = 6  // 6%
  let loan_years = 30
  
  // 计算总还款额（简化计算）
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(loan_amount, annual_interest_rate * loan_years), 100)
  let total_payment = @azimuth.add(loan_amount, total_interest)
  
  // 计算月供
  let total_months = @azimuth.multiply(loan_years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, total_months)
  
  @azimuth.assert_eq(270000, total_interest)  // 150000 * 6% * 30 = 270000
  @azimuth.assert_eq(420000, total_payment)   // 150000 + 270000 = 420000
  @azimuth.assert_eq(1167, monthly_payment)   // ceil(420000 / 360) = 1167
}

test "inventory_optimization_problem" {
  // 库存优化问题：多种商品的装箱计算
  // 商品A：23个，每箱装6个
  // 商品B：47个，每箱装8个
  // 商品C：91个，每箱装12个
  
  let boxes_a = @azimuth.divide_with_ceil(23, 6)  // 4箱
  let boxes_b = @azimuth.divide_with_ceil(47, 8)  // 6箱
  let boxes_c = @azimuth.divide_with_ceil(91, 12) // 8箱
  
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  let total_capacity_a = @azimuth.multiply(boxes_a, 6)  // 24
  let total_capacity_b = @azimuth.multiply(boxes_b, 8)  // 48
  let total_capacity_c = @azimuth.multiply(boxes_c, 12) // 96
  let total_capacity = @azimuth.add(@azimuth.add(total_capacity_a, total_capacity_b), total_capacity_c)
  let wasted_space = @azimuth.add(total_capacity, -(23 + 47 + 91))
  
  @azimuth.assert_eq(18, total_boxes)  // 4 + 6 + 8 = 18
  @azimuth.assert_eq(168, total_capacity)  // 24 + 48 + 96 = 168
  @azimuth.assert_eq(7, wasted_space)  // 168 - (23 + 47 + 91) = 7
}

test "extreme_boundary_values" {
  // 测试极端边界值
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 测试接近边界的运算
  @azimuth.assert_eq(2000000, @azimuth.add(max_safe_int, max_safe_int))
  @azimuth.assert_eq(-2000000, @azimuth.add(min_safe_int, min_safe_int))
  @azimuth.assert_eq(0, @azimuth.add(max_safe_int, min_safe_int))
  
  // 测试大数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_safe_int, max_safe_int))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(max_safe_int, 1))
  @azimuth.assert_eq(334, @azimuth.divide_with_ceil(max_safe_int, 2993))
}

test "complex_string_processing" {
  // 复杂字符串处理场景
  let company = "TechCorp"
  let department = "研发部"
  let project = "AI助手"
  
  // 构建复杂问候语
  let full_name = company + " " + department + " " + project
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, TechCorp 研发部 AI助手!", greeting)
  
  // 测试特殊字符组合
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
  @azimuth.assert_eq_string("Hello, 100%!", @azimuth.greet("100%"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "resource_allocation_algorithm" {
  // 资源分配算法：将资源分配给多个项目
  let total_resources = 1000
  let project_priority_1 = 350
  let project_priority_2 = 275
  let project_priority_3 = 180
  
  // 计算已分配资源
  let allocated = @azimuth.add(@azimuth.add(project_priority_1, project_priority_2), project_priority_3)
  
  // 计算剩余资源
  let remaining = @azimuth.add(total_resources, -allocated)
  
  // 将剩余资源平均分配给3个小项目
  let small_project_share = @azimuth.divide_with_ceil(remaining, 3)
  let total_small_projects = @azimuth.multiply(small_project_share, 3)
  
  @azimuth.assert_eq(805, allocated)  // 350 + 275 + 180 = 805
  @azimuth.assert_eq(195, remaining)  // 1000 - 805 = 195
  @azimuth.assert_eq(65, small_project_share)  // ceil(195 / 3) = 65
  @azimuth.assert_eq(195, total_small_projects)  // 65 * 3 = 195
}

test "time_series_calculation" {
  // 时间序列计算：计算项目进度
  let total_tasks = 157
  let team_members = 8
  let working_days = 15
  
  // 计算总工作能力
  let daily_capacity = @azimuth.multiply(team_members, 5)  // 每人每天5个任务
  let total_capacity = @azimuth.multiply(daily_capacity, working_days)
  
  // 计算需要的额外天数
  let days_needed = @azimuth.divide_with_ceil(total_tasks, daily_capacity)
  let extra_days = @azimuth.add(days_needed, -working_days)
  
  // 计算任务完成率
  let completion_rate = @azimuth.divide_with_ceil(@azimuth.multiply(total_capacity, 100), total_tasks)
  
  @azimuth.assert_eq(40, daily_capacity)  // 8 * 5 = 40
  @azimuth.assert_eq(600, total_capacity)  // 40 * 15 = 600
  @azimuth.assert_eq(4, days_needed)  // ceil(157 / 40) = 4
  @azimuth.assert_eq(-11, extra_days)  // 4 - 15 = -11 (表示提前完成)
  @azimuth.assert_eq(383, completion_rate)  // ceil(600 * 100 / 157) = 383
}