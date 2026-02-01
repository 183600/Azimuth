// 标准 MoonBit 质量测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "quality_add_comprehensive_test" {
  // 全面的加法测试
  assert_eq(0, add(0, 0))
  assert_eq(10, add(3, 7))
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(5, -5))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "quality_multiply_comprehensive_test" {
  // 全面的乘法测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(10, 10))
}

test "quality_divide_with_ceil_comprehensive_test" {
  // 全面的向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "quality_greet_comprehensive_test" {
  // 全面的问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "quality_mathematical_properties_test" {
  // 数学性质测试
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

test "quality_complex_business_scenario_test" {
  // 复杂业务场景测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "quality_packaging_calculation_test" {
  // 包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 测试不同的包装场景
  assert_eq(3, divide_with_ceil(15, 5))
  assert_eq(4, divide_with_ceil(19, 5))
  assert_eq(1, divide_with_ceil(3, 10))
}

test "quality_combined_operations_test" {
  // 组合运算测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(multiply(3, 4), multiply(2, 5)), 3)
  assert_eq(8, result3)  // ceil((12 + 10) / 3) = ceil(22/3) = 8
}

test "quality_edge_cases_test" {
  // 边界情况测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  
  // 测试零运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "quality_real_world_application_test" {
  // 实际应用测试：项目管理计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数
  let work_hours_per_day = 8
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  // 计算项目成本（每小时100元）
  let hourly_rate = 100
  let total_cost = multiply(total_work_hours, hourly_rate)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_work_hours)  // 6 * 8 = 48
  assert_eq(4800, total_cost)  // 48 * 100 = 4800
}