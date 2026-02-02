// 新增的高质量 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种场景和边缘情况

test "numeric_precision_and_range" {
  // 数值精度和范围测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 测试接近边界的数值
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  
  // 测试零值运算
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(2147483647, add(2147483647, 0))
}

test "complex_function_composition" {
  // 复合函数组合测试
  let base = 15
  
  // 复杂的函数组合计算
  let step1 = multiply(base, 3)        // 45
  let step2 = add(step1, 10)           // 55
  let step3 = divide_with_ceil(step2, 8)  // 7
  let step4 = multiply(step3, 2)       // 14
  let result = subtract(step4, base)   // -1
  
  assert_eq(-1, result)
  
  // 验证中间步骤
  assert_eq(45, step1)
  assert_eq(55, step2)
  assert_eq(7, step3)
  assert_eq(14, step4)
}

test "advanced_error_handling" {
  // 高级错误处理测试
  // 多种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483647, 0))
  
  // 极端除数测试
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(-2147483647, divide_with_ceil(-2147483647, 1))
  assert_eq(2147483647, divide_with_ceil(2147483647, 2147483647))
}

test "real_world_financial_calculations" {
  // 真实世界金融计算测试
  // 贷款计算场景
  let loan_amount = 250000
  let monthly_payment = 8500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(30, months_needed)  // ceil(250000/8500) = 30
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(255000, total_payment)
  
  // 计算利息
  let interest = subtract(total_payment, loan_amount)
  assert_eq(5000, interest)
  
  // 计算最后一个月的实际支付
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(5000, final_payment)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种特殊字符和格式
  assert_eq_string("Hello, MoonBit v0.1.0!", greet("MoonBit v0.1.0"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, https://example.com!", greet("https://example.com"))
  assert_eq_string("Hello, phone:123-456-7890!", greet("phone:123-456-7890"))
  assert_eq_string("Hello, special#$%^&*()chars!", greet("special#$%^&*()chars"))
  
  // 测试Unicode和emoji
  assert_eq_string("Hello, 🌟 MoonBit! 🚀", greet("🌟 MoonBit! 🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, ñoño español!", greet("ñoño español"))
}

test "mathematical_properties_comprehensive" {
  // 综合数学性质测试
  let a = 24
  let b = 18
  let c = 6
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律: (a + b) + c = a + (b + c)
  let associative_add_left = add(add(a, b), c)
  let associative_add_right = add(a, add(b, c))
  assert_eq(associative_add_left, associative_add_right)
  
  // 结合律: (a * b) * c = a * (b * c)
  let associative_mul_left = multiply(multiply(a, b), c)
  let associative_mul_right = multiply(a, multiply(b, c))
  assert_eq(associative_mul_left, associative_mul_right)
  
  // 验证具体数值
  assert_eq(288, distributive_left)  // 24 * (18 + 6) = 24 * 24 = 576
  assert_eq(576, distributive_right)  // 24 * 18 + 24 * 6 = 432 + 144 = 576
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：多项目资源分配
  let project_a_tasks = 45
  let project_b_tasks = 67
  let project_c_tasks = 23
  let team_capacity = 15
  
  // 计算每个项目需要的团队数
  let teams_a = divide_with_ceil(project_a_tasks, team_capacity)
  let teams_b = divide_with_ceil(project_b_tasks, team_capacity)
  let teams_c = divide_with_ceil(project_c_tasks, team_capacity)
  
  assert_eq(3, teams_a)  // ceil(45/15) = 3
  assert_eq(5, teams_b)  // ceil(67/15) = 5
  assert_eq(2, teams_c)  // ceil(23/15) = 2
  
  // 计算总团队数
  let total_teams = add(add(teams_a, teams_b), teams_c)
  assert_eq(10, total_teams)
  
  // 计算总任务数
  let total_tasks = add(add(project_a_tasks, project_b_tasks), project_c_tasks)
  assert_eq(135, total_tasks)
  
  // 计算总处理能力
  let total_capacity = multiply(total_teams, team_capacity)
  assert_eq(150, total_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(15, remaining_capacity)
}

test "performance_boundary_testing" {
  // 性能边界测试
  // 测试大数值的运算性能
  let large_num1 = 1000000
  let large_num2 = 999999
  
  // 大数加法
  let large_sum = add(large_num1, large_num2)
  assert_eq(1999999, large_sum)
  
  // 大数乘法
  let large_product = multiply(large_num1, 1000)
  assert_eq(1000000000, large_product)
  
  // 大数除法
  let large_division = divide_with_ceil(large_sum, 1000)
  assert_eq(2000, large_division)
  
  // 连续大数运算
  let complex_calc = divide_with_ceil(
    multiply(large_num1, 2),
    subtract(large_num2, 999000)
  )
  assert_eq(2000000, complex_calc)  // ceil(1000000*2/(999999-999000)) = ceil(2000000/999) = 2002
}

test "edge_case_comprehensive" {
  // 综合边缘情况测试
  // 最小值和最大值测试
  let max_int = 2147483647
  let min_int = -2147483647
  
  // 边界加法
  assert_eq(max_int, add(max_int - 1, 1))
  assert_eq(min_int, add(min_int + 1, -1))
  
  // 边界乘法
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
  assert_eq(0, divide_with_ceil(0, max_int))
  assert_eq(0, divide_with_ceil(0, min_int))
  
  // 特殊字符串测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
}

test "workflow_integration_test" {
  // 工作流集成测试
  // 模拟完整的业务工作流：订单处理
  
  // 订单信息
  let order_items = 127
  let items_per_box = 24
  let cost_per_item = 15
  let boxes_per_pallet = 4
  let shipping_cost_per_pallet = 200
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(6, boxes_needed)  // ceil(127/24) = 6
  
  // 计算商品总成本
  let item_cost_total = multiply(order_items, cost_per_item)
  assert_eq(1905, item_cost_total)  // 127 * 15 = 1905
  
  // 计算需要的托盘数
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(2, pallets_needed)  // ceil(6/4) = 2
  
  // 计算运输成本
  let shipping_total = multiply(pallets_needed, shipping_cost_per_pallet)
  assert_eq(400, shipping_total)  // 2 * 200 = 400
  
  // 计算订单总成本
  let total_order_cost = add(item_cost_total, shipping_total)
  assert_eq(2305, total_order_cost)  // 1905 + 400 = 2305
  
  // 计算总容量
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(144, total_box_capacity)  // 6 * 24 = 144
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_box_capacity, order_items)
  assert_eq(17, remaining_capacity)  // 144 - 127 = 17
  
  // 验证容量利用率
  let utilization_rate = divide_with_ceil(
    multiply(order_items, 100),
    total_box_capacity
  )
  assert_eq(89, utilization_rate)  // ceil(12700/144) = ceil(88.19) = 89%
}