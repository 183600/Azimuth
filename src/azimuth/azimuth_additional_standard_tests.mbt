// 额外的标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含10个高质量测试用例，覆盖核心功能

test "additional_basic_arithmetic_comprehensive" {
  // 综合基本算术测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(5, -8))
  assert_eq(0, add(0, 0))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-6, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(7, subtract(10, 3))
  assert_eq(-7, subtract(3, 10))
}

test "additional_divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况测试
  assert_eq(5, divide_with_ceil(20, 4))  // 整除
  assert_eq(5, divide_with_ceil(19, 4))  // 4.75 -> 5
  assert_eq(1, divide_with_ceil(1, 1))   // 自身
  assert_eq(1, divide_with_ceil(99, 100)) // 0.99 -> 1
  assert_eq(-4, divide_with_ceil(-15, 4)) // -3.75 -> -3
  assert_eq(4, divide_with_ceil(-15, -4)) // 3.75 -> 4
  assert_eq(0, divide_with_ceil(100, 0))  // 除零安全处理
}

test "additional_greet_function_comprehensive" {
  // 问候函数的综合测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // Emoji
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "additional_mathematical_properties_verification" {
  // 数学性质的验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 单位元性质
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "additional_real_world_business_calculations" {
  // 真实世界业务计算测试
  // 场景：电商订单计算
  let product_price = 89
  let quantity = 3
  let shipping_cost = 12
  let tax_rate = 8  // 8%
  let discount = 15
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(267, subtotal)
  assert_eq(22, tax)  // ceil(267 * 8 / 100) = ceil(21.36) = 22
  assert_eq(301, total_before_discount)
  assert_eq(286, final_total)
}

test "additional_resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：项目资源分配
  let total_budget = 5000
  let team_members = 8
  let project_duration_days = 15
  
  // 计算每人每日预算
  let total_person_days = multiply(team_members, project_duration_days)
  let budget_per_person_day = divide_with_ceil(total_budget, total_person_days)
  
  // 计算实际分配
  let allocated_budget = multiply(budget_per_person_day, total_person_days)
  let remaining_budget = subtract(total_budget, allocated_budget)
  
  assert_eq(120, total_person_days)
  assert_eq(42, budget_per_person_day)  // ceil(5000 / 120) = 42
  assert_eq(5040, allocated_budget)
  assert_eq(-40, remaining_budget)
}

test "additional_data_analysis_statistics" {
  // 数据分析统计测试
  // 场景：销售数据分析
  let sales_q1 = 45000
  let sales_q2 = 52000
  let sales_q3 = 48000
  let sales_q4 = 61000
  
  // 计算年度总销售额
  let annual_sales = add(add(add(sales_q1, sales_q2), sales_q3), sales_q4)
  
  // 计算季度平均销售额（向上取整）
  let quarterly_average = divide_with_ceil(annual_sales, 4)
  
  // 计算增长率
  let growth_q2_over_q1 = divide_with_ceil(multiply(sales_q2 - sales_q1, 100), sales_q1)
  let growth_annual = divide_with_ceil(multiply(annual_sales - multiply(sales_q1, 4), 100), multiply(sales_q1, 4))
  
  assert_eq(206000, annual_sales)
  assert_eq(51500, quarterly_average)
  assert_eq(16, growth_q2_over_q1)  // ceil(7000 * 100 / 45000) = 16
  assert_eq(14, growth_annual)     // ceil(26000 * 100 / 180000) = 14
}

test "additional_time_and_scheduling_calculations" {
  // 时间和调度计算测试
  // 场景：项目时间管理
  let total_tasks = 87
  let tasks_per_day = 6
  let work_days_per_week = 5
  let hours_per_day = 8
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数
  let total_hours = multiply(work_days_needed, hours_per_day)
  
  // 计算任务完成率（假设已完成65个任务）
  let completed_tasks = 65
  let completion_rate = divide_with_ceil(multiply(completed_tasks, 100), total_tasks)
  
  assert_eq(15, work_days_needed)    // ceil(87 / 6) = 15
  assert_eq(3, weeks_needed)         // ceil(15 / 5) = 3
  assert_eq(120, total_hours)        // 15 * 8 = 120
  assert_eq(75, completion_rate)     // ceil(65 * 100 / 87) = 75
}

test "additional_error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 测试极值情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483647, add(min_int, 1))
  assert_eq(0, add(max_int, -max_int))
  
  // 边界值乘法（使用较小的数避免溢出）
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
  assert_eq(0, multiply(max_int, 0))
  
  // 除零和极值除法
  assert_eq(0, divide_with_ceil(max_int, 0))
  assert_eq(0, divide_with_ceil(min_int, 0))
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
  
  // 空字符串和特殊字符串处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

test "additional_complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：完整的订单处理和库存管理流程
  
  // 产品信息
  let product_a_price = 25
  let product_a_qty = 8
  let product_b_price = 35
  let product_b_qty = 5
  
  // 库存信息
  let initial_stock_a = 50
  let initial_stock_b = 30
  let box_capacity = 20
  
  // 订单计算
  let subtotal_a = multiply(product_a_price, product_a_qty)
  let subtotal_b = multiply(product_b_price, product_b_qty)
  let order_total = add(subtotal_a, subtotal_b)
  
  // 库存更新
  let remaining_stock_a = subtract(initial_stock_a, product_a_qty)
  let remaining_stock_b = subtract(initial_stock_b, product_b_qty)
  let total_remaining_stock = add(remaining_stock_a, remaining_stock_b)
  
  // 重新包装计算
  let boxes_needed = divide_with_ceil(total_remaining_stock, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, total_remaining_stock)
  
  // 验证结果
  assert_eq(200, subtotal_a)     // 25 * 8 = 200
  assert_eq(175, subtotal_b)     // 35 * 5 = 175
  assert_eq(375, order_total)    // 200 + 175 = 375
  assert_eq(42, remaining_stock_a)  // 50 - 8 = 42
  assert_eq(25, remaining_stock_b)  // 30 - 5 = 25
  assert_eq(67, total_remaining_stock)  // 42 + 25 = 67
  assert_eq(4, boxes_needed)     // ceil(67 / 20) = 4
  assert_eq(80, total_capacity)  // 4 * 20 = 80
  assert_eq(13, unused_space)    // 80 - 67 = 13
}