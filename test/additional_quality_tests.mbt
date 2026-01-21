// 额外的高质量测试用例 - 针对 azimuth 库的全面测试
// 包含数学性质验证、边界情况和实际应用场景

test "mathematical_commutative_laws" {
  // 验证加法和乘法的交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证不同数值组合
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(multiply(-3, 7), multiply(7, -3))
}

test "mathematical_identity_elements" {
  // 验证数学中的单位元性质
  let test_value = 42
  
  // 加法单位元: a + 0 = a
  assert_eq(test_value, add(test_value, 0))
  assert_eq(test_value, add(0, test_value))
  
  // 乘法单位元: a * 1 = a
  assert_eq(test_value, multiply(test_value, 1))
  assert_eq(test_value, multiply(1, test_value))
  
  // 乘法零元: a * 0 = 0
  assert_eq(0, multiply(test_value, 0))
  assert_eq(0, multiply(0, test_value))
}

test "divide_with_ceil_fractional_cases" {
  // 测试除法向上取整的各种分数情况
  // 小于1的分数
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(1, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 接近整数的分数
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 大数的分数情况
  assert_eq(101, divide_with_ceil(10001, 100))
  assert_eq(51, divide_with_ceil(255, 5))
}

test "greet_function_multilingual_support" {
  // 测试多语言支持的问候功能
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
}

test "complex_business_inventory_calculation" {
  // 复杂业务场景：库存计算和成本分析
  let product_a_cost = 25
  let product_a_qty = 12
  let product_b_cost = 18
  let product_b_qty = 8
  let packaging_cost_per_box = 5
  let items_per_box = 10
  
  // 计算产品总成本
  let total_a_cost = multiply(product_a_cost, product_a_qty)
  let total_b_cost = multiply(product_b_cost, product_b_qty)
  let product_total_cost = add(total_a_cost, total_b_cost)
  
  // 计算需要的包装箱数量
  let total_items = add(product_a_qty, product_b_qty)
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算包装成本
  let packaging_total_cost = multiply(packaging_cost_per_box, boxes_needed)
  
  // 计算总成本
  let final_total_cost = add(product_total_cost, packaging_total_cost)
  
  assert_eq(300, total_a_cost)      // 25 * 12 = 300
  assert_eq(144, total_b_cost)      // 18 * 8 = 144
  assert_eq(444, product_total_cost) // 300 + 144 = 444
  assert_eq(2, boxes_needed)        // ceil(20/10) = 2
  assert_eq(10, packaging_total_cost) // 5 * 2 = 10
  assert_eq(454, final_total_cost)   // 444 + 10 = 454
}

test "mathematical_distributive_property_verification" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let test_values = [(1, 2, 3), (5, 7, 11), (10, 15, 20), (3, 8, 12)]
  
  for (a, b, c) in test_values {
    let left_side = multiply(a, add(b, c))
    let right_side = add(multiply(a, b), multiply(a, c))
    assert_eq(left_side, right_side)
  }
  
  // 特殊情况验证
  assert_eq(multiply(0, add(5, 10)), add(multiply(0, 5), multiply(0, 10)))
  assert_eq(multiply(5, add(0, 10)), add(multiply(5, 0), multiply(5, 10)))
}

test "edge_case_zero_and_negative_operations" {
  // 边界情况：零和负数的运算
  // 加法边界情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-5, -5))
  
  // 乘法边界情况
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(5, -5))
  
  // 除法边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "real_world_time_and_resource_planning" {
  // 实际应用：时间和资源规划
  let daily_working_hours = 8
  let tasks_per_hour = 3
  let total_tasks = 75
  let team_members = 4
  let vacation_days_per_member = 2
  
  // 计算每个成员的总任务数
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  // 计算每个成员需要的工作天数
  let daily_tasks_per_member = multiply(daily_working_hours, tasks_per_hour)
  let working_days_needed = divide_with_ceil(tasks_per_member, daily_tasks_per_member)
  
  // 计算包含假期的总天数
  let total_calendar_days = add(working_days_needed, vacation_days_per_member)
  
  // 验证计算结果
  assert_eq(19, tasks_per_member)         // ceil(75/4) = 19
  assert_eq(24, daily_tasks_per_member)    // 8 * 3 = 24
  assert_eq(1, working_days_needed)        // ceil(19/24) = 1
  assert_eq(3, total_calendar_days)        // 1 + 2 = 3
}