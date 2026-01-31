// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(3, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(0, -3))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  assert_true(remaining_space < box_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let days_per_task = 2
  let buffer_days = 3
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(40, total_tasks)
  
  // 计算基础天数
  let base_days = multiply(total_tasks, days_per_task)
  assert_eq(80, base_days)
  
  // 计算总项目天数
  let total_days = add(base_days, buffer_days)
  assert_eq(83, total_days)
  
  // 计算需要的冲刺数（每冲刺10天）
  let sprints_needed = divide_with_ceil(total_days, 10)
  assert_eq(9, sprints_needed)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "negative_numbers_comprehensive_test" {
  // 负数综合处理测试
  // 加法负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 乘法负数测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 减法负数测试
  assert_eq(-15, subtract(-10, 5))
  assert_eq(5, subtract(-10, -15))
  assert_eq(-20, subtract(-10, 10))
  
  // 除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 250
  let tables_available = 25
  let chairs_per_table = 8
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3840, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(160, remaining_budget)
}

test "data_processing_pipeline" {
  // 数据处理流水线测试
  // 场景：批量数据处理
  let data_batches = 150
  let batch_size = 12
  let processing_time_per_batch = 2
  let verification_time = 5
  
  // 计算需要的处理轮次
  let processing_rounds = divide_with_ceil(data_batches, batch_size)
  assert_eq(13, processing_rounds)
  
  // 计算总处理时间
  let total_processing_time = multiply(processing_rounds, processing_time_per_batch)
  assert_eq(26, total_processing_time)
  
  // 计算总时间（包括验证时间）
  let total_time = add(total_processing_time, verification_time)
  assert_eq(31, total_time)
  
  // 计算实际处理的数据量
  let actual_processed = multiply(processing_rounds - 1, batch_size)
  let final_batch_size = subtract(data_batches, actual_processed)
  assert_eq(6, final_batch_size)
  
  // 验证所有数据都被处理
  assert_eq(data_batches, add(actual_processed, final_batch_size))
}