// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖各种功能和场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let x = 15
  let y = 7
  
  // 基本运算
  @azimuth.assert_eq(22, @azimuth.add(x, y))
  @azimuth.assert_eq(105, @azimuth.multiply(x, y))
  @azimuth.assert_eq(8, @azimuth.subtract(x, y))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(x, y))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
  
  // 结合律验证
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(5, -3))
  
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-7, -5))
  
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
}

test "business_logic_packaging_simple" {
  // 业务逻辑：简单包装测试
  let product_1 = 47
  let box_size_1 = 12
  let boxes_1 = @azimuth.divide_with_ceil(product_1, box_size_1)
  @azimuth.assert_eq(4, boxes_1)
  
  let product_2 = 89
  let box_size_2 = 15
  let boxes_2 = @azimuth.divide_with_ceil(product_2, box_size_2)
  @azimuth.assert_eq(6, boxes_2)
  
  let total_boxes = @azimuth.add(boxes_1, boxes_2)
  @azimuth.assert_eq(10, total_boxes)
  
  let total_capacity = @azimuth.add(@azimuth.multiply(boxes_1, box_size_1), @azimuth.multiply(boxes_2, box_size_2))
  @azimuth.assert_eq(198, total_capacity)
}

test "string_processing_international_support" {
  // 字符串处理国际化支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, mixedCASE123!", @azimuth.greet("mixedCASE123"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划计算
  
  let team_size = 8
  let sprints = 5
  let tasks_per_sprint = 12
  let hours_per_task = 4
  let buffer_percentage = 20
  
  // 计算基础工作量
  let base_tasks = @azimuth.multiply(team_size, @azimuth.multiply(sprints, tasks_per_sprint))
  @azimuth.assert_eq(480, base_tasks)  // 8 * 5 * 12 = 480
  
  // 计算总工时
  let base_hours = @azimuth.multiply(base_tasks, hours_per_task)
  @azimuth.assert_eq(1920, base_hours)  // 480 * 4 = 1920
  
  // 添加缓冲时间
  let buffer_hours = @azimuth.divide_with_ceil(@azimuth.multiply(base_hours, buffer_percentage), 100)
  @azimuth.assert_eq(384, buffer_hours)  // ceil(1920 * 20 / 100) = 384
  
  // 计算总预估工时
  let total_hours = @azimuth.add(base_hours, buffer_hours)
  @azimuth.assert_eq(2304, total_hours)  // 1920 + 384 = 2304
  
  // 计算需要的周数（每周40小时）
  let weeks_needed = @azimuth.divide_with_ceil(total_hours, 40)
  @azimuth.assert_eq(58, weeks_needed)  // ceil(2304/40) = 58
}

test "edge_cases_and_boundary_values" {
  // 边界值和边缘情况测试
  // 极值测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 边界除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(9999, 10000))
  @azimuth.assert_eq(10000, @azimuth.divide_with_ceil(10000, 1))
  @azimuth.assert_eq(10001, @azimuth.divide_with_ceil(10001, 1))
  
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 零元素性质
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(100, @azimuth.subtract(100, 0))
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1000, 0))
  
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1000, 0))
  
  // 极值运算验证
  let max_val = 1000000
  let min_val = -1000000
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.subtract(max_val, 0))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议安排和资源分配
  
  let attendees = 250
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(32, tables_needed)  // ceil(250/8) = 32
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(6400, total_cost)
  
  // 计算剩余预算（或超支）
  let budget_difference = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(-400, budget_difference)  // 超支400
  
  // 计算需要的额外预算
  let additional_budget = @azimuth.multiply(-1, budget_difference)
  @azimuth.assert_eq(400, additional_budget)
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 场景：大数据量处理性能验证
  
  let data_size = 10000
  let batch_size = 75
  let processing_time_per_batch = 3
  let parallel_batches = 4
  
  // 计算需要的批次数
  let total_batches = @azimuth.divide_with_ceil(data_size, batch_size)
  @azimuth.assert_eq(134, total_batches)  // ceil(10000/75) = 134
  
  // 计算串行处理总时间
  let serial_time = @azimuth.multiply(total_batches, processing_time_per_batch)
  @azimuth.assert_eq(402, serial_time)  // 134 * 3 = 402
  
  // 计算并行处理时间
  let parallel_batches_needed = @azimuth.divide_with_ceil(total_batches, parallel_batches)
  @azimuth.assert_eq(34, parallel_batches_needed)  // ceil(134/4) = 34
  
  let parallel_time = @azimuth.multiply(parallel_batches_needed, processing_time_per_batch)
  @azimuth.assert_eq(102, parallel_time)  // 34 * 3 = 102
  
  // 计算性能提升
  let performance_improvement = @azimuth.subtract(serial_time, parallel_time)
  @azimuth.assert_eq(300, performance_improvement)  // 402 - 102 = 300
  
  // 计算加速比
  let speedup_ratio = @azimuth.divide_with_ceil(@azimuth.multiply(serial_time, 100), parallel_time)
  @azimuth.assert_eq(394, speedup_ratio)  // ceil(40200/102) = 394%
}