// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖各种功能和场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let x = 15
  let y = 7
  
  // 基本运算
  assert_eq(22, add(x, y))
  assert_eq(105, multiply(x, y))
  assert_eq(8, subtract(x, y))
  assert_eq(3, divide_with_ceil(x, y))
  
  // 边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
  
  // 结合律验证
  assert_eq(
    add(add(a, b), c),
    add(a, add(b, c))
  )
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-3, 8))
  
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(5, -3))
  
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(-2, divide_with_ceil(-7, 5))
  assert_eq(1, divide_with_ceil(-7, -5))
  
  assert_eq(-10, subtract(-5, 5))
  assert_eq(10, subtract(5, -5))
}

test "business_logic_packaging_simple" {
  // 业务逻辑：简单包装测试
  let product_1 = 47
  let box_size_1 = 12
  let boxes_1 = divide_with_ceil(product_1, box_size_1)
  assert_eq(4, boxes_1)
  
  let product_2 = 89
  let box_size_2 = 15
  let boxes_2 = divide_with_ceil(product_2, box_size_2)
  assert_eq(6, boxes_2)
  
  let total_boxes = add(boxes_1, boxes_2)
  assert_eq(10, total_boxes)
  
  let total_capacity = add(multiply(boxes_1, box_size_1), multiply(boxes_2, box_size_2))
  assert_eq(198, total_capacity)
}

test "string_processing_international_support" {
  // 字符串处理国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
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
  let base_tasks = multiply(team_size, multiply(sprints, tasks_per_sprint))
  assert_eq(480, base_tasks)  // 8 * 5 * 12 = 480
  
  // 计算总工时
  let base_hours = multiply(base_tasks, hours_per_task)
  assert_eq(1920, base_hours)  // 480 * 4 = 1920
  
  // 添加缓冲时间
  let buffer_hours = divide_with_ceil(multiply(base_hours, buffer_percentage), 100)
  assert_eq(384, buffer_hours)  // ceil(1920 * 20 / 100) = 384
  
  // 计算总预估工时
  let total_hours = add(base_hours, buffer_hours)
  assert_eq(2304, total_hours)  // 1920 + 384 = 2304
  
  // 计算需要的周数（每周40小时）
  let weeks_needed = divide_with_ceil(total_hours, 40)
  assert_eq(58, weeks_needed)  // ceil(2304/40) = 58
}

test "edge_cases_and_boundary_values" {
  // 边界值和边缘情况测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(10001, divide_with_ceil(10001, 1))
  
  // 除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 零元素性质
  assert_eq(100, add(100, 0))
  assert_eq(0, multiply(100, 0))
  assert_eq(100, subtract(100, 0))
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(1000, 0))
  
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(-1000, 0))
  
  // 极值运算验证
  let max_val = 1000000
  let min_val = -1000000
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, subtract(max_val, 0))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议安排和资源分配
  
  let attendees = 250
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(250/8) = 32
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6400, total_cost)
  
  // 计算剩余预算（或超支）
  let budget_difference = subtract(total_budget, total_cost)
  assert_eq(-400, budget_difference)  // 超支400
  
  // 计算需要的额外预算
  let additional_budget = multiply(-1, budget_difference)
  assert_eq(400, additional_budget)
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 场景：大数据量处理性能验证
  
  let data_size = 10000
  let batch_size = 75
  let processing_time_per_batch = 3
  let parallel_batches = 4
  
  // 计算需要的批次数
  let total_batches = divide_with_ceil(data_size, batch_size)
  assert_eq(134, total_batches)  // ceil(10000/75) = 134
  
  // 计算串行处理总时间
  let serial_time = multiply(total_batches, processing_time_per_batch)
  assert_eq(402, serial_time)  // 134 * 3 = 402
  
  // 计算并行处理时间
  let parallel_batches_needed = divide_with_ceil(total_batches, parallel_batches)
  assert_eq(34, parallel_batches_needed)  // ceil(134/4) = 34
  
  let parallel_time = multiply(parallel_batches_needed, processing_time_per_batch)
  assert_eq(102, parallel_time)  // 34 * 3 = 102
  
  // 计算性能提升
  let performance_improvement = subtract(serial_time, parallel_time)
  assert_eq(300, performance_improvement)  // 402 - 102 = 300
  
  // 计算加速比
  let speedup_ratio = divide_with_ceil(multiply(serial_time, 100), parallel_time)
  assert_eq(394, speedup_ratio)  // ceil(40200/102) = 394%
}