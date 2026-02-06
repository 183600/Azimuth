// 高质量测试套件 - 为 Azimuth 项目新增的标准 MoonBit 测试用例
// 包含10个精心设计的测试用例，覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(42, add(20, 22))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(100000, add(50000, 50000))
  assert_eq(-8, add(-3, -5))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(144, multiply(12, 12))
  assert_eq(0, multiply(999, 0))
  assert_eq(-49, multiply(-7, 7))
  assert_eq(64, multiply(-8, -8))
  assert_eq(1, multiply(1, 1000000))
}

test "divide_with_ceil_complex_scenarios" {
  // 向上取整除法复杂场景测试
  assert_eq(17, divide_with_ceil(100, 6))
  assert_eq(1, divide_with_ceil(7, 7))
  assert_eq(0, divide_with_ceil(0, 123))
  assert_eq(0, divide_with_ceil(45, 0))
  assert_eq(-16, divide_with_ceil(-95, 6))
}

test "subtract_function_various_cases" {
  // 减法函数各种情况测试
  assert_eq(25, subtract(100, 75))
  assert_eq(-30, subtract(20, 50))
  assert_eq(0, subtract(42, 42))
  assert_eq(150, subtract(50, -100))
  assert_eq(-200, subtract(-100, 100))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 5
  let b = 8
  let c = 3
  
  // a * (b + c) = a * b + a * c
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
  
  // 验证具体数值: 5 * (8 + 3) = 5 * 8 + 5 * 3 = 55
  assert_eq(55, multiply(5, add(8, 3)))
}

test "resource_allocation_calculation" {
  // 资源分配计算测试
  let total_tasks = 87
  let team_members = 8
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(11, tasks_per_member)
  
  // 验证总分配任务数
  let total_allocated = multiply(tasks_per_member, team_members)
  assert_eq(88, total_allocated)
  
  // 验证剩余任务槽位
  let remaining_slots = subtract(total_allocated, total_tasks)
  assert_eq(1, remaining_slots)
}

test "financial_calculation_workflow" {
  // 财务计算工作流测试
  let item_price = 47
  let quantity = 9
  let subtotal = multiply(item_price, quantity)
  
  let tax_rate = 8
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(423, subtotal)  // 47 * 9 = 423
  assert_eq(34, tax_amount)  // ceil(423 * 8 / 100) = ceil(33.84) = 34
  assert_eq(457, total)  // 423 + 34 = 457
}

test "time_management_scenario" {
  // 时间管理场景测试
  let total_minutes = 125
  let minutes_per_hour = 60
  let hours_needed = divide_with_ceil(total_minutes, minutes_per_hour)
  let remaining_minutes = subtract(total_minutes, multiply(hours_needed - 1, minutes_per_hour))
  
  assert_eq(3, hours_needed)
  assert_eq(5, remaining_minutes)
  
  // 验证总时间计算正确
  let calculated_total = add(multiply(hours_needed - 1, minutes_per_hour), remaining_minutes)
  assert_eq(total_minutes, calculated_total)
}

test "inventory_optimization_test" {
  // 库存优化测试
  let current_stock = 234
  let safety_stock = 50
  let reorder_point = add(current_stock, safety_stock)
  
  let package_size = 24
  let packages_to_order = divide_with_ceil(reorder_point, package_size)
  
  assert_eq(284, reorder_point)
  assert_eq(12, packages_to_order)
  
  // 验证新库存水平
  let new_stock = add(current_stock, multiply(packages_to_order, package_size))
  assert_eq(522, new_stock)
  
  // 验证满足安全库存要求
  assert_true(new_stock >= reorder_point)
}