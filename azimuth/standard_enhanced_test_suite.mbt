// 标准 MoonBit 增强测试套件
// 包含10个高质量测试用例，覆盖各种场景和边缘情况

test "mathematical_operations_comprehensive" {
  // 数学运算综合测试：测试多个函数的组合使用
  let a = 15
  let b = 4
  let c = 7
  
  // 复合运算测试
  let result1 = add(multiply(a, b), divide_with_ceil(c, 2))
  assert_eq(64, result1)  // 15*4 + ceil(7/2) = 60 + 4 = 64
  
  let result2 = subtract(multiply(a, c), divide_with_ceil(b, 3))
  assert_eq(103, result2)  // 15*7 - ceil(4/3) = 105 - 2 = 103
  
  let result3 = divide_with_ceil(add(a, b), c)
  assert_eq(3, result3)  // ceil(19/7) = 3
}

test "boundary_conditions_extreme" {
  // 边界条件极值测试：测试系统在极值情况下的表现
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  
  // 零值处理
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1000000, add(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
}

test "string_processing_advanced" {
  // 字符串处理高级测试：测试各种字符串输入情况
  // 特殊字符和符号
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  
  // Unicode 和表情符号
  assert_eq_string("Hello, 🌙⭐☀️!", greet("🌙⭐☀️"))
  assert_eq_string("Hello, 🏳️‍🌈🌍!", greet("🏳️‍🌈🌍"))
  
  // 空白字符
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 
	!", greet("
	"))
  
  // 数字和字母混合
  assert_eq_string("Hello, ABC123xyz!", greet("ABC123xyz"))
}

test "business_logistics_simulation" {
  // 业务物流模拟测试：模拟真实的物流和库存管理场景
  let total_products = 287
  let box_capacity = 24
  let pallet_capacity = 8  // 每个托盘放8箱
  let truck_capacity = 5   // 每辆卡车放5个托盘
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(12, boxes_needed)  // ceil(287/24) = 12
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, pallet_capacity)
  assert_eq(2, pallets_needed)  // ceil(12/8) = 2
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(pallets_needed, truck_capacity)
  assert_eq(1, trucks_needed)  // ceil(2/5) = 1
  
  // 验证总容量
  let total_box_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(288, total_box_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_box_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试：测试系统对各种错误情况的处理
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值组合测试
  let max_positive = 2147483647
  let max_negative = -2147483648
  
  assert_eq(0, multiply(max_positive, 0))
  assert_eq(0, multiply(max_negative, 0))
  assert_eq(max_positive, add(max_positive, 0))
  assert_eq(max_negative, add(max_negative, 0))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(max_positive, max_positive))
  assert_eq(1, divide_with_ceil(max_negative, max_negative))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试：验证基本数学定律
  let x = 42
  let y = 17
  let z = 8
  
  // 加法交换律：a + b = b + a
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(x, add(y, z)), add(add(x, y), z))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, multiply(y, z)), multiply(multiply(x, y), z))
  
  // 分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)
  
  // 恒等式验证
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, subtract(x, x))
}

test "performance_large_scale_computation" {
  // 性能大规模计算测试：测试系统在大量数据计算时的表现
  let large_number = 100000
  let iterations = 100
  
  // 批量加法计算
  let sum_result = multiply(large_number, iterations)
  assert_eq(10000000, sum_result)
  
  // 批量乘法计算
  let product_result = multiply(large_number, large_number)
  assert_eq(10000000000, product_result)
  
  // 复杂计算链
  let step1 = add(large_number, divide_with_ceil(large_number, 10))
  let step2 = multiply(step1, divide_with_ceil(large_number, 100))
  let step3 = divide_with_ceil(step2, iterations)
  assert_eq(11000, step3)  // ceil(ceil((100000 + 10000) * 1000) / 100)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试：测试多步骤复杂计算流程
  // 场景：计算项目资源需求和成本分析
  let team_size = 12
  let project_duration_days = 45
  let hours_per_day = 8
  let hourly_rate = 75
  let overhead_percentage = 20
  let budget_limit = 500000
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(project_duration_days, hours_per_day))
  assert_eq(4320, total_hours)  // 12 * 45 * 8 = 4320
  
  // 计算基础成本
  let base_cost = multiply(total_hours, hourly_rate)
  assert_eq(324000, base_cost)  // 4320 * 75 = 324000
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_percentage), 100)
  assert_eq(64800, overhead_cost)  // ceil(324000 * 20 / 100) = 64800
  
  // 计算总成本
  let total_cost = add(base_cost, overhead_cost)
  assert_eq(388800, total_cost)  // 324000 + 64800 = 388800
  
  // 计算剩余预算
  let remaining_budget = subtract(budget_limit, total_cost)
  assert_eq(111200, remaining_budget)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_cost, 100), budget_limit)
  assert_eq(78, budget_utilization)  // ceil(38880000 / 500000) = 78%
}

test "data_validation_integrity" {
  // 数据验证完整性测试：测试输入输出数据的有效性和一致性
  // 数值范围验证
  // 验证加法一致性
  assert_eq(0, add(0, 0))
  assert_eq(2, add(1, 1))
  assert_eq(-2, add(-1, -1))
  assert_eq(0, add(1, -1))
  assert_eq(0, add(-1, 1))
  
  // 验证乘法一致性
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(-1, multiply(-1, 1))
  
  // 验证除法一致性
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(1, 0))
  
  // 字符串长度验证
  let empty_string = greet("")
  assert_eq_string("Hello, !", empty_string)
  
  let single_char = greet("A")
  assert_eq_string("Hello, A!", single_char)
  
  let long_string = "This is a very long string to test the greet function"
  let expected_long = "Hello, " + long_string + "!"
  assert_eq_string(expected_long, greet(long_string))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：模拟具体的业务需求和解决方案
  // 场景：电商订单处理和库存管理
  let product_a_qty = 150
  let product_b_qty = 280
  let product_c_qty = 95
  
  let box_capacity_a = 24
  let box_capacity_b = 36
  let box_capacity_c = 18
  
  let shipping_container_capacity = 50  // 每个集装箱可容纳50箱
  
  // 计算各产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity_c)
  
  assert_eq(7, boxes_a)   // ceil(150/24) = 7
  assert_eq(8, boxes_b)   // ceil(280/36) = 8
  assert_eq(6, boxes_c)   // ceil(95/18) = 6
  
  // 计算总箱子数量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(21, total_boxes)
  
  // 计算需要的集装箱数量
  let containers_needed = divide_with_ceil(total_boxes, shipping_container_capacity)
  assert_eq(1, containers_needed)  // ceil(21/50) = 1
  
  // 计算各产品的实际容量
  let actual_capacity_a = multiply(boxes_a, box_capacity_a)
  let actual_capacity_b = multiply(boxes_b, box_capacity_b)
  let actual_capacity_c = multiply(boxes_c, box_capacity_c)
  
  assert_eq(168, actual_capacity_a)  // 7 * 24
  assert_eq(288, actual_capacity_b)  // 8 * 36
  assert_eq(108, actual_capacity_c)  // 6 * 18
  
  // 计算总容量
  let total_capacity = add(add(actual_capacity_a, actual_capacity_b), actual_capacity_c)
  assert_eq(564, total_capacity)
  
  // 计算剩余空间
  let total_products = add(add(product_a_qty, product_b_qty), product_c_qty)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(39, remaining_space)  // 564 - (150 + 280 + 95) = 564 - 525 = 39
  
  // 计算空间利用率
  let space_utilization = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, space_utilization)  // ceil(52500 / 564) = 94%
}