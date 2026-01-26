// Azimuth 专注测试用例
// 针对 azimuth 库核心功能的高质量测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
  
  // 测试零的情况
  assert_eq(add(0, 25), add(25, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 7
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(2, divide_with_ceil(4, 2))   // 4/2 = 2 -> 2
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-3, divide_with_ceil(-11, 4))  // -11/4 = -2.75 -> -2
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的 Unicode 字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🌍!", greet("🚀🌍"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_scenario_calculation" {
  // 复杂业务场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, pre_discount_total)
  assert_eq(372, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let team_a_cost = 1200
  let team_b_cost = 800
  let team_c_cost = 1500
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个项目成本300）
  let small_projects_count = divide_with_ceil(remaining_budget, 300)
  
  assert_eq(3500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(5, small_projects_count)
}

test "time_management_calculation" {
  // 时间管理计算测试
  let total_tasks = 67
  let tasks_per_day = 8
  let work_hours_per_day = 8
  let meeting_hours_per_day = 2
  
  // 计算需要的工作天数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算实际工作小时数（扣除会议时间）
  let productive_hours_per_day = add(work_hours_per_day, -meeting_hours_per_day)
  let total_productive_hours = multiply(work_days_needed, productive_hours_per_day)
  
  assert_eq(9, work_days_needed)
  assert_eq(6, productive_hours_per_day)
  assert_eq(54, total_productive_hours)
}

test "mathematical_series_fibonacci_style" {
  // 类斐波那契数列计算
  let first = 1
  let second = 1
  let third = add(first, second)
  let fourth = add(second, third)
  let fifth = add(third, fourth)
  let sixth = add(fourth, fifth)
  
  // 验证数列值
  assert_eq(2, third)
  assert_eq(3, fourth)
  assert_eq(5, fifth)
  assert_eq(8, sixth)
  
  // 计数列和
  let series_sum = add(add(add(add(first, second), third), add(fourth, fifth)), sixth)
  assert_eq(20, series_sum)
}

test "error_boundary_and_extreme_values" {
  // 错误边界和极值测试
  // 测试零的各种运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1000))
  assert_eq(0, multiply(-500, 0))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 测试极值组合
  assert_eq(100001, add(large_num, 1))
  assert_eq(99999, add(large_num, -1))
}