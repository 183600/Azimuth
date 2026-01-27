// Azimuth 库的综合单元测试 - 10个测试用例
// 测试覆盖核心功能的各种场景

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))  // 加法交换律
  assert_eq(multiply(a, b), multiply(b, a))  // 乘法交换律
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 相等数字
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 向上取整
  assert_eq(0, divide_with_ceil(0, 100))  // 零除以正数
  assert_eq(0, divide_with_ceil(50, 0))  // 除数为零的安全处理
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax_amount)
  assert_eq(609, total)
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "resource_allocation_scenario" {
  // 资源分配场景计算
  let total_budget = 5000
  let project_cost = 1200
  let num_projects = 3
  
  let total_project_cost = multiply(project_cost, num_projects)
  let remaining_budget = add(total_budget, -total_project_cost)
  let additional_small_projects = divide_with_ceil(remaining_budget, 300)
  
  assert_eq(3600, total_project_cost)
  assert_eq(1400, remaining_budget)
  assert_eq(5, additional_small_projects)
}

test "time_management_calculations" {
  // 时间管理计算
  let tasks_per_day = 6
  let total_tasks = 47
  let work_days = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(work_days, 8)
  
  assert_eq(8, work_days)  // ceil(47/6) = 8
  assert_eq(64, total_work_hours)  // 8 * 8 = 64
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))  // 邮箱格式
}

test "mathematical_series_calculation" {
  // 数学级数计算
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  
  // 计算等差数列第n项: a + (n-1)*d
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  
  // 计算等差数列和: n/2 * (2a + (n-1)*d)
  let sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(20, nth_term)  // 5 + 5*3 = 20
  assert_eq(75, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75
}

test "inventory_optimization" {
  // 库存优化计算
  let current_stock = 250
  let daily_demand = 18
  let lead_time_days = 7
  let safety_stock = 50
  
  // 计算再订货点
  let demand_during_lead = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead, safety_stock)
  
  // 计算订货量（满足30天需求）
  let monthly_demand = multiply(daily_demand, 30)
  let order_quantity = add(monthly_demand, safety_stock)
  
  assert_eq(126, demand_during_lead)
  assert_eq(176, reorder_point)
  assert_eq(590, order_quantity)
}