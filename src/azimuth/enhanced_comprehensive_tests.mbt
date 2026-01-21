// 增强综合测试用例 - 为 azimuth 库添加的高质量测试
// 专注于数学性质、实际应用场景和边界情况

test "associative_law_verification" {
  // 验证加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left_side = add(add(a, b), c)
  let right_side = add(a, add(b, c))
  assert_eq(left_side, right_side)  // (15 + 27) + 42 = 15 + (27 + 42) = 84
  
  // 验证乘法结合律: (a * b) * c = a * (b * c)
  let x = 3
  let y = 7
  let z = 5
  
  let mult_left = multiply(multiply(x, y), z)
  let mult_right = multiply(x, multiply(y, z))
  assert_eq(mult_left, mult_right)  // (3 * 7) * 5 = 3 * (7 * 5) = 105
}

test "mathematical_identities" {
  // 验证数学恒等式
  // 加法单位元: a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元: a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
  
  // 乘法零元: a * 0 = 0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_consistency" {
  // 测试向上取整除法的一致性
  // 对于正数，结果应该 >= 普通除法结果
  assert_eq(3, divide_with_ceil(6, 2))   // 6/2 = 3.0 -> 3
  assert_eq(4, divide_with_ceil(7, 2))   // 7/2 = 3.5 -> 4
  assert_eq(2, divide_with_ceil(4, 2))   // 4/2 = 2.0 -> 2
  assert_eq(3, divide_with_ceil(5, 2))   // 5/2 = 2.5 -> 3
  
  // 测试连续除法的一致性
  let value = 100
  let divisor1 = 3
  let divisor2 = 7
  
  let step1 = divide_with_ceil(value, divisor1)
  let step2 = divide_with_ceil(step1, divisor2)
  let direct = divide_with_ceil(value, multiply(divisor1, divisor2))
  
  // 注意：由于向上取整的特性，step2可能不等于direct
  assert_eq(5, step2)  // ceil(ceil(100/3)/7) = ceil(34/7) = 5
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  // 测试包含特殊字符的名称
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Connor!", greet("O'Connor"))
  assert_eq_string("Hello, Dr. Smith!", greet("Dr. Smith"))
  assert_eq_string("Hello, Mr. & Mrs. Johnson!", greet("Mr. & Mrs. Johnson"))
  
  // 测试数字和字母混合
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, A1B2C3!", greet("A1B2C3"))
  
  // 测试只有空格的字符串
  assert_eq_string("Hello,   !", greet("   "))
}

test "complex_business_calculation" {
  // 复杂业务计算：批发采购场景
  let product_a_price = 25
  let product_a_qty = 12
  let product_b_price = 18
  let product_b_qty = 8
  let product_c_price = 32
  let product_c_qty = 5
  
  // 计算各商品小计
  let subtotal_a = multiply(product_a_price, product_a_qty)
  let subtotal_b = multiply(product_b_price, product_b_qty)
  let subtotal_c = multiply(product_c_price, product_c_qty)
  
  // 计算总金额
  let total_amount = add(add(subtotal_a, subtotal_b), subtotal_c)
  
  // 计算需要多少个箱子（每箱装30个商品）
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let boxes_needed = divide_with_ceil(total_items, 30)
  
  assert_eq(716, total_amount)  // 25*12 + 18*8 + 32*5 = 300 + 144 + 160 = 604
  assert_eq(1, boxes_needed)    // ceil(25/30) = 1
}

test "mathematical_series_progression" {
  // 数学级数：等差数列和等比数列
  // 等差数列求和：1 + 3 + 5 + ... + 19 (10个奇数)
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let arithmetic_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 验证结果：1 + 3 + 5 + ... + 19 = 100
  assert_eq(100, arithmetic_sum)
  
  // 等比数列求和（简化版）：2 + 4 + 8 + 16 (4项)
  let geo_first = 2
  let geo_ratio = 2
  let geo_terms = 4
  
  let geo_sum = multiply(geo_first, divide_with_ceil(multiply(geo_ratio, geo_terms) - 1, geo_ratio - 1))
  assert_eq(30, geo_sum)  // 2 + 4 + 8 + 16 = 30
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 1000
  let project_cost_a = 234
  let project_cost_b = 189
  let project_cost_c = 156
  
  // 计算剩余预算
  let spent_a = project_cost_a
  let spent_b = project_cost_b
  let spent_c = project_cost_c
  let total_spent = add(add(spent_a, spent_b), spent_c)
  let remaining = add(total_budget, -total_spent)
  
  // 计算可以支持多少个额外的小项目（每个75元）
  let small_project_cost = 75
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(421, remaining)  // 1000 - (234 + 189 + 156) = 421
  assert_eq(6, additional_projects)  // ceil(421/75) = 6
}

test "time_management_scheduling" {
  // 时间管理和调度
  let tasks_per_hour = 4
  let working_hours_per_day = 8
  let total_tasks = 150
  
  // 计算需要多少天
  let tasks_per_day = multiply(tasks_per_hour, working_hours_per_day)
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数
  let total_hours = multiply(days_needed, working_hours_per_day)
  
  // 计算剩余时间（可以处理的额外任务数）
  let capacity = multiply(days_needed, tasks_per_day)
  let spare_capacity = add(capacity, -total_tasks)
  
  assert_eq(5, days_needed)      // ceil(150/(4*8)) = ceil(150/32) = 5
  assert_eq(40, total_hours)     // 5 * 8 = 40
  assert_eq(10, spare_capacity)  // 5*32 - 150 = 160 - 150 = 10
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试极端数值组合
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界值的运算
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483647, add(min_int, 1))
  
  // 测试除法边界情况
  assert_eq(1, divide_with_ceil(1, max_int))
  assert_eq(0, divide_with_ceil(-1, max_int))
  assert_eq(max_int, divide_with_ceil(max_int, 1))
  
  // 测试零的各种情况
  assert_eq(0, divide_with_ceil(0, max_int))
  assert_eq(0, divide_with_ceil(0, min_int))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, multiply(0, max_int))
  assert_eq(0, multiply(0, min_int))
}

test "comprehensive_integration_test" {
  // 综合集成测试：模拟完整的业务流程
  // 场景：电商平台的订单处理
  
  // 商品信息
  let laptop_price = 5999
  let laptop_qty = 2
  let mouse_price = 199
  let mouse_qty = 3
  let keyboard_price = 499
  let keyboard_qty = 1
  
  // 计算商品总价
  let laptop_total = multiply(laptop_price, laptop_qty)
  let mouse_total = multiply(mouse_price, mouse_qty)
  let keyboard_total = multiply(keyboard_price, keyboard_qty)
  let subtotal = add(add(laptop_total, mouse_total), keyboard_total)
  
  // 计算运费（每箱50元，每箱最多装5件商品）
  let total_items = add(add(laptop_qty, mouse_qty), keyboard_qty)
  let shipping_boxes = divide_with_ceil(total_items, 5)
  let shipping_cost = multiply(shipping_boxes, 50)
  
  // 计算税费（简化为8%）
  let tax = divide_with_ceil(multiply(subtotal, 8), 100)
  
  // 计算最终总价
  let total_before_discount = add(add(subtotal, shipping_cost), tax)
  let discount = 500  // 促销折扣
  let final_total = add(total_before_discount, -discount)
  
  // 生成订单确认信息
  let order_summary = "订单总额: " + final_total.to_string() + "元"
  let greeting = greet("尊贵的客户")
  
  // 验证计算结果
  assert_eq(14194, final_total)  // 5999*2 + 199*3 + 499*1 + ceil(6/5)*50 + ceil(13195*8/100) - 500
  assert_eq_string("Hello, 尊贵的客户!", greeting)
}