// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 包的核心功能

test "add_function_zero_properties" {
  // 测试加法函数的零元素性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, add(0, -5))
}

test "multiply_function_identity_properties" {
  // 测试乘法函数的单位元性质
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "greet_function_empty_and_special_inputs" {
  // 测试问候函数的空输入和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"!", greet("\""))
  assert_eq_string("Hello, \\", greet("\\"))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目成本和资源分配
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  
  // 计算每个阶段的成本（分4个阶段）
  let cost_per_phase = divide_with_ceil(total_cost, 4)
  
  assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  assert_eq(57600, total_cost)  // 768 * 75 = 57600
  assert_eq(14400, cost_per_phase)  // ceil(57600/4) = 14400
}

test "mathematical_commutative_and_associative" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：服务器资源分配
  let total_memory = 32768  // MB
  let servers = 7
  let memory_per_server = divide_with_ceil(total_memory, servers)
  
  assert_eq(4682, memory_per_server)  // ceil(32768/7) = 4682
  
  // 计算实际分配的总内存
  let total_allocated = multiply(memory_per_server, servers)
  let memory_waste = subtract(total_allocated, total_memory)
  
  assert_eq(32774, total_allocated)
  assert_eq(6, memory_waste)
  
  // 优化分配策略
  let optimized_servers = add(servers, 2)
  let optimized_memory_per_server = divide_with_ceil(total_memory, optimized_servers)
  assert_eq(3277, optimized_memory_per_server)  // ceil(32768/9)
}

test "real_world_logistics_scenario" {
  // 真实世界物流场景测试
  // 场景：货物包装和运输
  let product_a_quantity = 125
  let product_b_quantity = 87
  let product_c_quantity = 156
  
  let box_capacity_a = 12
  let box_capacity_b = 8
  let box_capacity_c = 15
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_quantity, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_quantity, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_quantity, box_capacity_c)
  
  assert_eq(11, boxes_a)  // ceil(125/12) = 11
  assert_eq(11, boxes_b)  // ceil(87/8) = 11
  assert_eq(11, boxes_c)  // ceil(156/15) = 11
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(33, total_boxes)
  
  // 计算运输成本（每个箱子5元）
  let cost_per_box = 5
  let total_cost = multiply(total_boxes, cost_per_box)
  assert_eq(165, total_cost)
  
  // 计算总容量
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  assert_eq(425, total_capacity)
  
  // 计算剩余空间
  let total_items = add(add(product_a_quantity, product_b_quantity), product_c_quantity)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(57, remaining_space)
}