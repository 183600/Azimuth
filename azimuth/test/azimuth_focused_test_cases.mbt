// Azimuth 专注测试用例 - 8个高质量标准 MoonBit 测试用例
// 覆盖实际业务场景和边缘情况

test "resource_allocation_calculation" {
  // 资源分配计算测试
  // 场景：计算项目所需的服务器数量
  let total_users = 1250
  let users_per_server = 100
  let servers_needed = divide_with_ceil(total_users, users_per_server)
  
  assert_eq(13, servers_needed)
  
  // 验证总容量
  let total_capacity = multiply(servers_needed, users_per_server)
  assert_eq(1300, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_users)
  assert_eq(50, remaining_capacity)
}

test "financial_calculation_with_rounding" {
  // 金融计算与舍入测试
  // 场景：计算分期付款
  let total_amount = 1250
  let installments = 3
  let base_payment = divide_with_ceil(total_amount, installments)
  
  assert_eq(417, base_payment)
  
  // 验证总支付金额
  let total_payment = multiply(base_payment, installments)
  assert_eq(1251, total_payment)
  
  // 计算额外支付
  let extra_payment = subtract(total_payment, total_amount)
  assert_eq(1, extra_payment)
}

test "time_management_workflow" {
  // 时间管理工作流测试
  // 场景：计算完成任务所需的工作日
  let total_hours = 87
  let hours_per_day = 8
  let days_needed = divide_with_ceil(total_hours, hours_per_day)
  
  assert_eq(11, days_needed)
  
  // 计算实际工作时间
  let actual_hours = multiply(days_needed - 1, hours_per_day)
  let remaining_hours = subtract(total_hours, actual_hours)
  assert_eq(7, remaining_hours)
}

test "inventory_optimization" {
  // 库存优化测试
  // 场景：计算最优订货批量
  let monthly_demand = 450
  let package_size = 24
  let packages_needed = divide_with_ceil(monthly_demand, package_size)
  
  assert_eq(19, packages_needed)
  
  // 验证总库存
  let total_inventory = multiply(packages_needed, package_size)
  assert_eq(456, total_inventory)
  
  // 计算安全库存
  let safety_stock = subtract(total_inventory, monthly_demand)
  assert_eq(6, safety_stock)
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  // 场景：计算多项式表达式
  let x = 5
  let y = 3
  
  // 计算: (x + y) * (x - y) + 2*x
  let step1 = add(x, y)           // 8
  let step2 = subtract(x, y)      // 2
  let step3 = multiply(step1, step2)  // 16
  let step4 = multiply(2, x)      // 10
  let result = add(step3, step4)  // 26
  
  assert_eq(26, result)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 测试各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  let max_int = 1000000
  let min_int = -1000000
  
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, max_int))
  assert_eq(-1, divide_with_ceil(-1, max_int))
  assert_eq(1, divide_with_ceil(max_int, max_int))
}

test "batch_processing_optimization" {
  // 批处理优化测试
  // 场景：计算批处理任务的批次数量
  let total_items = 987
  let optimal_batch_size = 75
  let batches_needed = divide_with_ceil(total_items, optimal_batch_size)
  
  assert_eq(14, batches_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(batches_needed, optimal_batch_size)
  assert_eq(1050, total_capacity)
  
  // 计算剩余处理能力
  let unused_capacity = subtract(total_capacity, total_items)
  assert_eq(63, unused_capacity)
  
  // 计算利用率
  let utilization = multiply(100, divide_with_ceil(total_items, total_capacity))
  assert_eq(94, utilization)  // floor(98700/1050) = 94%
}