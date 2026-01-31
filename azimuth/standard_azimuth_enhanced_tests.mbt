// 标准 Azimuth 增强测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖不同场景和边界情况

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-10, 10))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-40, multiply(-5, 8))
  assert_eq(35, multiply(-5, -7))
  
  assert_eq(3, divide_with_ceil(25, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "boundary_conditions_extreme" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "negative_numbers_comprehensive" {
  // 负数处理综合测试
  // 负数加法
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-8, 10))
  
  // 负数乘法
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-20, multiply(4, -5))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(-2, divide_with_ceil(15, -8))
  assert_eq(2, divide_with_ceil(-15, -8))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_logic_packaging_optimization" {
  // 业务逻辑：包装优化测试
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(94, utilization_percent)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1920, total_tasks)
  
  // 计算需要的冲刺数（每冲刺400个任务）
  let tasks_per_sprint = 400
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(5, sprints_needed)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算
  let max_safe = 1000000
  let min_safe = -1000000
  
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(0, multiply(min_safe, 0))
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(min_safe, add(min_safe, 0))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：会议室分配
  let participants = 87
  let rooms_available = 6
  let capacity_per_room = 15
  
  // 计算需要的房间数量
  let rooms_needed = divide_with_ceil(participants, capacity_per_room)
  assert_eq(6, rooms_needed)
  
  // 验证房间是否足够
  assert_true(rooms_needed <= rooms_available)
  
  // 计算总座位数
  let total_seats = multiply(rooms_needed, capacity_per_room)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
}

test "performance_scalability_validation" {
  // 性能和可扩展性验证测试
  // 测试不同规模的数据处理
  let small_scale = 100
  let medium_scale = 1000
  let large_scale = 10000
  
  // 线性增长验证
  let small_result = multiply(small_scale, 5)
  let medium_result = multiply(medium_scale, 5)
  let large_result = multiply(large_scale, 5)
  
  assert_eq(500, small_result)
  assert_eq(5000, medium_result)
  assert_eq(50000, large_result)
  
  // 批处理效率验证
  let batch_size = 25
  let small_batches = divide_with_ceil(small_scale, batch_size)
  let medium_batches = divide_with_ceil(medium_scale, batch_size)
  let large_batches = divide_with_ceil(large_scale, batch_size)
  
  assert_eq(4, small_batches)
  assert_eq(40, medium_batches)
  assert_eq(400, large_batches)
  
  // 验证批处理数量的线性关系
  assert_eq(multiply(small_batches, 10), medium_batches)
  assert_eq(multiply(medium_batches, 10), large_batches)
}