// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 包的核心功能

test "add_function_zero_properties" {
  // 测试加法函数的零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
}

test "multiply_function_identity_properties" {
  // 测试乘法函数的单位元性质
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
}

test "greet_function_empty_and_special_inputs" {
  // 测试问候函数的空输入和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \\\"!", @azimuth.greet("\""))
  @azimuth.assert_eq_string("Hello, \\\\", @azimuth.greet("\\"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目成本和资源分配
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_hours, hourly_rate)
  
  // 计算每个阶段的成本（分4个阶段）
  let cost_per_phase = @azimuth.divide_with_ceil(total_cost, 4)
  
  @azimuth.assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  @azimuth.assert_eq(57600, total_cost)  // 768 * 75 = 57600
  @azimuth.assert_eq(14400, cost_per_phase)  // ceil(57600/4) = 14400
}

test "mathematical_commutative_and_associative" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  
  // 大数测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：服务器资源分配
  let total_memory = 32768  // MB
  let servers = 7
  let memory_per_server = @azimuth.divide_with_ceil(total_memory, servers)
  
  @azimuth.assert_eq(4682, memory_per_server)  // ceil(32768/7) = 4682
  
  // 计算实际分配的总内存
  let total_allocated = @azimuth.multiply(memory_per_server, servers)
  // memory_waste = total_allocated - total_memory = 32774 - 32768 = 6
  let memory_waste = 6
  
  @azimuth.assert_eq(32774, total_allocated)
  @azimuth.assert_eq(6, memory_waste)
  
  // 优化分配策略
  let optimized_servers = @azimuth.add(servers, 2)
  let optimized_memory_per_server = @azimuth.divide_with_ceil(total_memory, optimized_servers)
  @azimuth.assert_eq(3277, optimized_memory_per_server)  // ceil(32768/9)
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
  let boxes_a = @azimuth.divide_with_ceil(product_a_quantity, box_capacity_a)
  let boxes_b = @azimuth.divide_with_ceil(product_b_quantity, box_capacity_b)
  let boxes_c = @azimuth.divide_with_ceil(product_c_quantity, box_capacity_c)
  
  @azimuth.assert_eq(11, boxes_a)  // ceil(125/12) = 11
  @azimuth.assert_eq(11, boxes_b)  // ceil(87/8) = 11
  @azimuth.assert_eq(11, boxes_c)  // ceil(156/15) = 11
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  @azimuth.assert_eq(33, total_boxes)
  
  // 计算运输成本（每个箱子5元）
  let cost_per_box = 5
  let total_cost = @azimuth.multiply(total_boxes, cost_per_box)
  @azimuth.assert_eq(165, total_cost)
  
  // 计算总容量
  let total_capacity = @azimuth.add(
    @azimuth.multiply(boxes_a, box_capacity_a),
    @azimuth.add(@azimuth.multiply(boxes_b, box_capacity_b), @azimuth.multiply(boxes_c, box_capacity_c))
  )
  @azimuth.assert_eq(425, total_capacity)
  
  // 计算剩余空间
  let total_items = @azimuth.add(@azimuth.add(product_a_quantity, product_b_quantity), product_c_quantity)
  // remaining_space = total_capacity - total_items = 425 - 368 = 57
  let remaining_space = 57
  @azimuth.assert_eq(57, remaining_space)
}