// 高质量测试套件 - 针对 azimuth 库的全面测试
// 使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(-3, 4))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(16, multiply(-4, -4))
  assert_eq(1, multiply(1, 1000))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 35
  let quantity = 6
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(210, subtotal)
  assert_eq(222, total_before_discount)
  assert_eq(202, final_total)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 1500
  let project_cost = 275
  let projects_funded = divide_with_ceil(total_budget, project_cost)
  
  assert_eq(6, projects_funded)
  
  let actual_spending = multiply(project_cost, projects_funded)
  assert_eq(1650, actual_spending)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 7
  let total_tasks = 50
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(8, days_needed)
  
  let total_work_hours = multiply(days_needed, 8)
  assert_eq(64, total_work_hours)
}

test "complex_nested_operations" {
  // 复杂嵌套运算
  let base_value = 15
  let multiplier = 3
  let offset = 7
  
  let result = add(multiply(base_value, multiplier), offset)
  assert_eq(52, result)  // 15*3 + 7 = 45 + 7 = 52
  
  let nested_result = multiply(add(base_value, offset), multiplier)
  assert_eq(66, nested_result)  // (15 + 7)*3 = 22*3 = 66
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1000000, multiply(1000, 1000))
}