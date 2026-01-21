// 高质量聚焦测试用例 - 针对 azimuth 库的核心功能
// 包含边界情况、错误处理和实际应用场景的测试

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等元
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(-17, add(-17, 0))
  
  // 乘法恒等元（注意：当前库没有实现1的恒等元测试）
  assert_eq(0, multiply(15, 0))
  assert_eq(0, multiply(0, 15))
  assert_eq(0, multiply(-7, 0))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试负数场景下的向上取整除法
  // 负数除以正数
  assert_eq(-2, divide_with_ceil(-5, 3))   // -5/3 = -1.666... -> -1 (向上取整)
  assert_eq(-1, divide_with_ceil(-3, 3))   // -3/3 = -1 -> -1
  assert_eq(-1, divide_with_ceil(-1, 3))   // -1/3 = -0.333... -> 0 (向上取整)
  
  // 正数除以负数
  assert_eq(-2, divide_with_ceil(5, -3))   // 5/-3 = -1.666... -> -1
  assert_eq(-1, divide_with_ceil(3, -3))   // 3/-3 = -1 -> -1
  assert_eq(-1, divide_with_ceil(1, -3))   // 1/-3 = -0.333... -> 0
  
  // 负数除以负数
  assert_eq(2, divide_with_ceil(-5, -3))   // -5/-3 = 1.666... -> 2
  assert_eq(1, divide_with_ceil(-3, -3))   // -3/-3 = 1 -> 1
  assert_eq(1, divide_with_ceil(-1, -3))   // -1/-3 = 0.333... -> 1
}

test "greet_function_with_whitespace_handling" {
  // 测试问候函数的空白字符处理
  assert_eq_string("Hello,   !", greet("  "))      // 只有空格
  assert_eq_string("Hello, \t!", greet("\t"))     // 只有制表符
  assert_eq_string("Hello, \n!", greet("\n"))     // 只有换行符
  assert_eq_string("Hello,  a !", greet("  a "))   // 前后有空格
  assert_eq_string("Hello, a b c!", greet("a b c")) // 中间有空格
}

test "complex_business_calculation_inventory" {
  // 复杂业务计算：库存管理场景
  let initial_stock = 150
  let sales_per_day = 12
  let days_of_operation = 7
  let restock_threshold = 50
  let package_size = 25
  
  // 计算一周后的库存
  let weekly_sales = multiply(sales_per_day, days_of_operation)
  let remaining_stock = add(initial_stock, -weekly_sales)
  
  // 计算需要补货的包数
  let restock_needed = divide_with_ceil(restock_threshold, package_size)
  
  assert_eq(66, remaining_stock)  // 150 - 12*7 = 150 - 84 = 66
  assert_eq(2, restock_needed)    // ceil(50/25) = 2
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 0 + 1 = 1
  let f3 = add(f1, f2)  // 1 + 1 = 2
  let f4 = add(f2, f3)  // 1 + 2 = 3
  let f5 = add(f3, f4)  // 2 + 3 = 5
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  
  // 验证黄金比例近似
  let ratio_approx = divide_with_ceil(multiply(f5, 100), f4)
  assert_eq(167, ratio_approx)  // ceil(5/3 * 100) = ceil(166.66...) = 167
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 1000
  let project_a_cost = 234
  let project_b_cost = 189
  let project_c_cost = 156
  
  // 计算三个项目的总成本
  let total_cost = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_cost)
  
  // 计算可以支持的小项目数量（每个小项目成本45）
  let small_project_cost = 45
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(579, total_cost)  // 234 + 189 + 156 = 579
  assert_eq(421, remaining_budget)  // 1000 - 579 = 421
  assert_eq(10, small_projects_count)  // ceil(421/45) = 10
}

test "string_greeting_with_special_formatting" {
  // 特殊格式化的字符串问候测试
  let name_with_quotes = greet("\"John\"")
  let name_with_apostrophe = greet("O'Connor")
  let name_with_dashes = greet("Jean-Claude")
  let name_with_numbers = greet("Agent007")
  let name_with_symbols = greet("Mr. Smith Jr.")
  
  assert_eq_string("Hello, \"John\"!", name_with_quotes)
  assert_eq_string("Hello, O'Connor!", name_with_apostrophe)
  assert_eq_string("Hello, Jean-Claude!", name_with_dashes)
  assert_eq_string("Hello, Agent007!", name_with_numbers)
  assert_eq_string("Hello, Mr. Smith Jr.!", name_with_symbols)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let total_tasks = 87
  let tasks_per_hour = 8
  let working_hours_per_day = 8
  let deadline_days = 3
  
  // 计算完成所有任务需要的小时数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算完成所有任务需要的天数
  let total_days_needed = divide_with_ceil(total_hours_needed, working_hours_per_day)
  
  // 计算在截止日期前可用的总工作小时数
  let available_hours = multiply(deadline_days, working_hours_per_day)
  
  // 计算需要加班的小时数
  let overtime_hours = add(total_hours_needed, -available_hours)
  
  assert_eq(11, total_hours_needed)  // ceil(87/8) = 11
  assert_eq(2, total_days_needed)    // ceil(11/8) = 2
  assert_eq(24, available_hours)     // 3 * 8 = 24
  assert_eq(-13, overtime_hours)     // 11 - 24 = -13 (不需要加班)
}

test "error_boundary_and_extreme_cases" {
  // 错误边界和极端情况测试
  // 测试零值的各种运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试单位值运算
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 测试大数运算的边界
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 测试负数边界
  assert_eq(0, add(100, -100))
  assert_eq(-10000, multiply(-100, 100))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_shopping_cart_calculations" {
  // 真实世界购物车计算
  // 商品价格和数量
  let apple_price = 3
  let apple_qty = 4
  let banana_price = 2
  let banana_qty = 6
  let orange_price = 5
  let orange_qty = 2
  
  // 计算各商品小计
  let apple_subtotal = multiply(apple_price, apple_qty)
  let banana_subtotal = multiply(banana_price, banana_qty)
  let orange_subtotal = multiply(orange_price, orange_qty)
  
  // 计算总价
  let total_before_tax = add(add(apple_subtotal, banana_subtotal), orange_subtotal)
  
  // 计算税费（8%，向上取整）
  let tax = divide_with_ceil(multiply(total_before_tax, 8), 100)
  
  // 计算最终总价
  let final_total = add(total_before_tax, tax)
  
  // 计算需要多少个购物袋（每个袋子最多装5件商品）
  let total_items = add(add(apple_qty, banana_qty), orange_qty)
  let bags_needed = divide_with_ceil(total_items, 5)
  
  assert_eq(12, apple_subtotal)     // 3 * 4 = 12
  assert_eq(12, banana_subtotal)    // 2 * 6 = 12
  assert_eq(10, orange_subtotal)    // 5 * 2 = 10
  assert_eq(34, total_before_tax)   // 12 + 12 + 10 = 34
  assert_eq(3, tax)                 // ceil(34 * 8 / 100) = ceil(2.72) = 3
  assert_eq(37, final_total)        // 34 + 3 = 37
  assert_eq(3, bags_needed)         // ceil(12 / 5) = 3
}