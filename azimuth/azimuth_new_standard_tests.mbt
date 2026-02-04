// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖各种场景

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let a = 5
  let b = 3
  let c = 2
  
  // 测试复合运算：(a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)  // (5 + 3) * 2 = 16
  
  // 测试复合运算：a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)  // 5 * 3 + 2 = 17
  
  // 测试复合运算：ceil((a + b) / c)
  let result3 = divide_with_ceil(add(a, b), c)
  assert_eq(4, result3)  // ceil(8/2) = 4
}

test "resource_allocation_algorithm" {
  // 资源分配算法测试
  let total_tasks = 47
  let team_members = 5
  let working_days = 8
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_members, working_days)
  let tasks_per_person_per_day = divide_with_ceil(total_tasks, total_person_days)
  
  assert_eq(2, tasks_per_person_per_day)  // ceil(47/40) = 2
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_per_day, total_person_days)
  assert_eq(80, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(33, remaining_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, CAPS!", greet("CAPS"))
  assert_eq_string("Hello, mixedCase!", greet("mixedCase"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
  assert_eq_string("Hello, spaces !", greet("spaces "))
  assert_eq_string("Hello,  spaces!", greet("  spaces"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  // 加法负数测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(0, add(-5, 5))
  
  // 乘法负数测试
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 减法负数测试
  assert_eq(-2, subtract(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(5, -3))
  
  // 除法负数测试
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "business_inventory_optimization" {
  // 业务库存优化测试
  let product_a = 23
  let product_b = 31
  let product_c = 17
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(2, boxes_a)  // ceil(23/12) = 2
  assert_eq(3, boxes_b)  // ceil(31/12) = 3
  assert_eq(2, boxes_c)  // ceil(17/12) = 2
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(7, total_boxes)
  assert_eq(84, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(71, total_products)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(13, remaining_space)
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, add(5, 0))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(10001, divide_with_ceil(10001, 1))
}

test "event_planning_complex_scenario" {
  // 活动规划复杂场景测试
  let attendees = 156
  let tables_available = 20
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 5000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(1000, remaining_budget)
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_cost, 100), total_budget)
  assert_eq(80, budget_utilization)  // floor(400000/5000) = 80%
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  
  // 极值运算测试
  let large_positive = 100000
  let large_negative = -100000
  
  assert_eq(0, multiply(large_positive, 0))
  assert_eq(0, multiply(large_negative, 0))
  assert_eq(large_positive, add(large_positive, 0))
  assert_eq(large_negative, add(large_negative, 0))
  
  // 边界减法测试
  assert_eq(0, subtract(100000, 100000))
  assert_eq(200000, subtract(100000, -100000))
  assert_eq(-200000, subtract(-100000, 100000))
}

test "performance_calculation_workflow" {
  // 性能计算工作流测试
  let cpu_cores = 8
  let threads_per_core = 2
  let tasks_per_thread = 5
  let task_duration_minutes = 30
  let total_time_limit_hours = 8
  
  // 计算总线程数
  let total_threads = multiply(cpu_cores, threads_per_core)
  assert_eq(16, total_threads)
  
  // 计算总任务数
  let total_tasks = multiply(total_threads, tasks_per_thread)
  assert_eq(80, total_tasks)
  
  // 计算总任务时间（分钟）
  let total_task_time_minutes = multiply(total_tasks, task_duration_minutes)
  assert_eq(2400, total_task_time_minutes)
  
  // 计算总任务时间（小时）
  let total_task_time_hours = divide_with_ceil(total_task_time_minutes, 60)
  assert_eq(40, total_task_time_hours)
  
  // 计算需要的并行运行轮数
  let parallel_rounds = divide_with_ceil(total_task_time_hours, total_time_limit_hours)
  assert_eq(5, parallel_rounds)
  
  // 计算实际完成时间
  let actual_completion_hours = multiply(parallel_rounds, total_time_limit_hours)
  assert_eq(40, actual_completion_hours)
}

test "data_analysis_algorithm" {
  // 数据分析算法测试
  let dataset_size = 1000
  let sample_size = 75
  let processing_batches = 8
  let error_margin_percent = 5
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(dataset_size, sample_size)
  assert_eq(14, batches_needed)  // ceil(1000/75) = 14
  
  // 计算每批次的实际处理量
  let actual_batch_size = divide_with_ceil(dataset_size, batches_needed)
  assert_eq(72, actual_batch_size)  // ceil(1000/14) = 72
  
  // 计算总处理能力
  let total_processing_capacity = multiply(batches_needed, actual_batch_size)
  assert_eq(1008, total_processing_capacity)
  
  // 计算冗余处理量
  let redundant_processing = subtract(total_processing_capacity, dataset_size)
  assert_eq(8, redundant_processing)
  
  // 计算处理效率
  let processing_efficiency = divide_with_ceil(multiply(dataset_size, 100), total_processing_capacity)
  assert_eq(99, processing_efficiency)  // floor(100000/1008) = 99%
  
  // 验证处理效率是否在可接受范围内
  assert_true(processing_efficiency >= subtract(100, error_margin_percent))
}