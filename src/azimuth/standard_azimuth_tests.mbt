// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(1000, -999))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(-15, multiply(3, -5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_mathematical_properties" {
  // 测试向上取整除法的数学性质
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_laws_verification" {
  // 验证基本数学定律
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_before_discount)
  assert_eq(211, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(9999, 100))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let final_stock = add(initial_stock, -total_sold)
  
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(84, total_sold)
  assert_eq(66, final_stock)
  assert_eq(5, boxes_needed)
}