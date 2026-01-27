// 高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的综合测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, María!", greet("María"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_scenario" {
  // 复杂业务场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_cost)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_boxes = multiply(boxes_needed, 1)
  let remaining_space = multiply(total_boxes, items_per_box) - total_items
  
  assert_eq(4, boxes_needed)
  assert_eq(4, total_boxes)
  assert_eq(1, remaining_space)
}

test "time_management_scenario" {
  // 时间管理场景
  let tasks_per_day = 8
  let total_tasks = 35
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(5, work_days_needed)
  assert_eq(40, total_hours)
}

test "mathematical_series_calculation" {
  // 数学级数计算
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 5
  
  // 使用公式计算等差数列和：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(35, series_sum)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
  assert_eq(1000000, add(500000, 500000))
}