// 用户请求的 MoonBit 测试用例 - Azimuth 项目
// 覆盖更复杂的业务场景和边界情况

test "user_financial_calculation_scenario" {
  // 财务计算场景：计算商品总价和税费
  let unit_price = 120
  let quantity = 8
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整到最近的10元）
  let tax_rate = 5
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(960, subtotal)  // 120 * 8 = 960
  assert_eq(48, tax_amount)  // ceil(960 * 5 / 100) = ceil(48) = 48
  assert_eq(1008, total)  // 960 + 48 = 1008
}

test "user_time_management_scenario" {
  // 时间管理场景：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作周（每周5个工作日）
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(15, days_needed)  // ceil(87/6) = 15
  assert_eq(3, weeks_needed)  // ceil(15/5) = 3
}

test "user_resource_allocation_scenario" {
  // 资源分配场景：计算服务器资源需求
  let users_per_server = 50
  let total_users = 237
  let servers_needed = divide_with_ceil(total_users, users_per_server)
  
  // 计算备用服务器（10%的额外容量）
  let backup_servers = divide_with_ceil(multiply(servers_needed, 10), 100)
  let total_servers = add(servers_needed, backup_servers)
  
  assert_eq(5, servers_needed)  // ceil(237/50) = 5
  assert_eq(1, backup_servers)  // ceil(5*10/100) = ceil(0.5) = 1
  assert_eq(6, total_servers)  // 5 + 1 = 6
}

test "user_mathematical_distributive_property" {
  // 数学分配律测试：a * (b + c) = a*b + a*c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7*(12+8) = 7*12 + 7*8 = 140
}

test "user_extreme_boundary_conditions" {
  // 极端边界条件测试
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试极端除法情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(0, divide_with_ceil(0, 1000000))
}

test "user_string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试特殊字符
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \\\"quoted\\\"!", greet("\\\"quoted\\\""))
  
  // 测试长字符串
  let long_name = "VeryLongNameThatMightCauseIssuesInSomeSystems"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "user_complex_calculation_chain" {
  // 复杂计算链测试
  let base_value = 10
  
  // 第一步：加上乘积
  let step1 = add(base_value, multiply(3, 4))  // 10 + 3*4 = 22
  
  // 第二步：减去除法结果
  let step2 = subtract(step1, divide_with_ceil(15, 4))  // 22 - ceil(15/4) = 22 - 4 = 18
  
  // 第三步：乘以加法结果
  let step3 = multiply(step2, add(2, 1))  // 18 * (2+1) = 18 * 3 = 54
  
  // 第四步：加上向上取整的除法
  let final_result = add(step3, divide_with_ceil(step3, 10))  // 54 + ceil(54/10) = 54 + 6 = 60
  
  assert_eq(60, final_result)
}

test "user_inventory_optimization_scenario" {
  // 库存优化场景：计算最优订货量
  let monthly_demand = 450
  let order_cost = 50
  let holding_cost_per_unit = 2
  
  // 计算经济订货量 (EOQ) 的简化版本
  let eoq_numerator = multiply(monthly_demand, multiply(order_cost, 2))
  let eoq = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  // 计算年订货次数
  let annual_demand = multiply(monthly_demand, 12)
  let orders_per_year = divide_with_ceil(annual_demand, eoq)
  
  assert_eq(45000, eoq_numerator)  // 450 * (50 * 2) = 450 * 100 = 45000
  assert_eq(22500, eoq)  // ceil(45000 / 2) = 22500
  assert_eq(1, orders_per_year)  // ceil(5400 / 22500) = ceil(0.24) = 1
}

test "user_boolean_logic_simulation" {
  // 布尔逻辑模拟：使用数值运算模拟布尔操作
  let true_val = 1
  let false_val = 0
  
  // 模拟 AND 操作：a && b = a * b
  let and_true = multiply(true_val, true_val)
  let and_false1 = multiply(true_val, false_val)
  let and_false2 = multiply(false_val, true_val)
  let and_false3 = multiply(false_val, false_val)
  
  // 模拟 OR 操作：a || b = a + b - (a * b)
  let or_true1 = subtract(add(true_val, false_val), multiply(true_val, false_val))
  let or_true2 = subtract(add(false_val, true_val), multiply(false_val, true_val))
  let or_true3 = subtract(add(true_val, true_val), multiply(true_val, true_val))
  let or_false = subtract(add(false_val, false_val), multiply(false_val, false_val))
  
  assert_eq(1, and_true)
  assert_eq(0, and_false1)
  assert_eq(0, and_false2)
  assert_eq(0, and_false3)
  
  assert_eq(1, or_true1)
  assert_eq(1, or_true2)
  assert_eq(1, or_true3)
  assert_eq(0, or_false)
}

test "user_error_handling_and_recovery" {
  // 错误处理和恢复场景测试
  // 测试除零错误的处理
  let normal_division = divide_with_ceil(10, 2)
  let zero_division = divide_with_ceil(10, 0)
  let negative_zero_division = divide_with_ceil(-10, 0)
  
  // 测试负数除法的边界情况
  let negative_dividend = divide_with_ceil(-10, 3)
  let negative_divisor = divide_with_ceil(10, -3)
  let both_negative = divide_with_ceil(-10, -3)
  
  assert_eq(5, normal_division)
  assert_eq(0, zero_division)
  assert_eq(0, negative_zero_division)
  assert_eq(-3, negative_dividend)
  assert_eq(-4, negative_divisor)
  assert_eq(4, both_negative)
}
