// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(46, add(17, 29))
  assert_eq(493, multiply(17, 29))
}

test "divide_with_ceil_mixed_operations" {
  // 测试向上取整除法的混合运算
  let total_items = 58
  let package_size = 15
  let packages_needed = divide_with_ceil(total_items, package_size)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, package_size)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(60, total_capacity)
  assert_eq(2, unused_space)
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：计算订单总价（含税、运费、折扣）
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping_cost = 25
  let discount_threshold = 500
  let discount_amount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(534, subtotal)
  assert_eq(43, tax_amount)
  assert_eq(602, pre_discount_total)
  assert_eq(552, final_total)
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  // 零元素测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(-12345, 0))
  assert_eq(12345, add(12345, 0))
  assert_eq(-12345, add(-12345, 0))
  
  // 单位元测试
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(-12345, 1))
  
  // 相反数测试
  assert_eq(0, add(12345, -12345))
  assert_eq(0, add(-12345, 12345))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 完全整除的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(7, divide_with_ceil(21, 3))
  assert_eq(-5, divide_with_ceil(-10, 2))
  
  // 需要向上取整的情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(8, divide_with_ceil(22, 3))
  assert_eq(-5, divide_with_ceil(-11, 2))
  
  // 大数精度测试
  assert_eq(1001, divide_with_ceil(1000001, 1000))
  assert_eq(334, divide_with_ceil(1000000, 3000))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_costs = [1500, 2000, 1200, 1800, 2500]
  
  // 计算项目总成本
  let total_project_cost = add(add(add(add(project_costs[0], project_costs[1]), project_costs[2]), project_costs[3]), project_costs[4])
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_project_cost)
  
  // 计算可以支持的小项目数量（每个小项目成本800）
  let small_projects_count = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(9000, total_project_cost)
  assert_eq(1000, remaining_budget)
  assert_eq(2, small_projects_count)
}

test "time_series_calculation" {
  // 测试时间序列计算
  // 场景：计算项目完成时间
  let total_tasks = 125
  let team_members = 8
  let tasks_per_member_per_day = 3
  let work_days_per_week = 5
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_member_per_day)
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(24, daily_capacity)
  assert_eq(6, work_days_needed)
  assert_eq(2, weeks_needed)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  assert_eq(100000000000, multiply(large_positive, large_positive))
  assert_eq(100000000000, multiply(large_negative, large_negative))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  
  // 数字和特殊符号
  assert_eq_string("Hello, 123456789!", greet("123456789"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
  
  // 长字符串
  assert_eq_string("Hello, This is a very long string that tests the greet function's ability to handle extended text input!", greet("This is a very long string that tests the greet function's ability to handle extended text input!"))
}