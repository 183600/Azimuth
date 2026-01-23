// 综合标准测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "arithmetical_identity_elements" {
  // 测试算术单位元性质
  assert_eq(5, add(5, 0))  // 加法单位元
  assert_eq(-3, add(-3, 0))
  assert_eq(7, multiply(7, 1))  // 乘法单位元
  assert_eq(-10, multiply(-10, 1))
  assert_eq(0, multiply(5, 0))  // 零元素性质
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(0, 5))     // 零除以正数
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello,   !", greet("  "))       // 空格字符串
  assert_eq_string("Hello, 123!", greet("123"))     // 数字字符串
  assert_eq_string("Hello, 世界!", greet("世界"))   // Unicode字符
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 12
  let y = 8
  let z = 5
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 结合律验证
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律验证
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let base_price = 150
  let quantity = 8
  let tax_rate = 12  // 12%
  let shipping_fee = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(base_price, quantity)
  
  // 计算税费（使用向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_fee)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1200, subtotal)
  assert_eq(144, tax_amount)
  assert_eq(1369, total_before_discount)
  assert_eq(1319, final_total)
}

test "extreme_boundary_values" {
  // 测试极端边界值
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 接近Int边界的值
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  
  // 大数除法精度
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(666667, divide_with_ceil(2000000, 3))
}

test "resource_allocation_optimization" {
  // 资源分配优化场景
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let reserve_percentage = 15
  
  // 计算项目总成本
  let projects_total = add(project_a_cost, project_b_cost)
  
  // 计算预留资金
  let reserve_funds = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  
  // 计算可用于其他项目的资金
  let available_funds = add(total_budget, -add(projects_total, reserve_funds))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 450
  let small_projects_count = divide_with_ceil(available_funds, small_project_cost)
  
  assert_eq(6000, projects_total)
  assert_eq(1500, reserve_funds)
  assert_eq(2500, available_funds)
  assert_eq(6, small_projects_count)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let tasks_per_day = 6
  let total_tasks = 47
  let working_days_per_week = 5
  let hours_per_day = 8
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  // 计算总工作小时数
  let total_hours = multiply(workdays_needed, hours_per_day)
  
  assert_eq(8, workdays_needed)  // ceil(47/6) = 8
  assert_eq(2, weeks_needed)     // ceil(8/5) = 2
  assert_eq(64, total_hours)     // 8 * 8 = 64
}

test "data_analysis_and_statistics" {
  // 数据分析和统计计算
  let data_count = 5
  
  // 计算总和（手动计算）
  let sum = add(add(add(add(125, 150), 175), 200), 225)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, data_count)
  
  // 计算中位数（已排序数组）
  let median = 175
  
  // 计算范围
  let range = add(225, -125)
  
  assert_eq(875, sum)
  assert_eq(175, average)
  assert_eq(175, median)
  assert_eq(100, range)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数运算的一致性
  let negative_result = multiply(-5, 3)
  let positive_result = multiply(5, -3)
  assert_eq(negative_result, positive_result)
  
  // 零值运算的一致性
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(0, 0))
  
  // 字符串处理的健壮性
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, special@chars#123!", greet("special@chars#123"))
}