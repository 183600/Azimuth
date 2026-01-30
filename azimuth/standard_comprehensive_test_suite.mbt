// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  // 测试加法的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
  
  // 测试乘法的各种情况
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 123))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试减法的各种情况
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(7, 7))
  assert_eq(10, subtract(7, -3))
  assert_eq(-10, subtract(-7, 3))
}

test "divide_with_ceil_boundary_conditions" {
  // 向上取整除法的边界条件测试
  // 基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 边界值测试
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数测试
  assert_eq(1000, divide_with_ceil(10000, 10))
  assert_eq(1001, divide_with_ceil(10001, 10))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  let z = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
  
  // 恒等式验证
  assert_eq(x, add(x, 0))  // 加法恒等式
  assert_eq(x, multiply(x, 1))  // 乘法恒等式
  assert_eq(0, multiply(x, 0))  // 零乘法性质
}

test "string_processing_international" {
  // 字符串处理国际化测试
  // 基本功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, a!", greet("a"))
  
  // 国际字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // Emoji 和特殊字符测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  
  // 数字和符号测试
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  // 负数加法测试
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
  
  // 负数乘法测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 负数减法测试
  assert_eq(-10, subtract(-5, 5))
  assert_eq(-15, subtract(-5, 10))
  assert_eq(5, subtract(-5, -10))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  // 场景1：计算装盒需要的箱子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
  
  // 场景2：多产品包装计算
  let product_a = 25
  let product_b = 30
  let product_c = 18
  let box_capacity = 20
  
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(2, boxes_a)  // ceil(25/20) = 2
  assert_eq(2, boxes_b)  // ceil(30/20) = 2
  assert_eq(1, boxes_c)  // ceil(18/20) = 1
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(5, total_boxes)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_size = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  let budget_limit = 50000
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(48000, total_budget)  // 960 * 50 = 48000
  
  // 验证预算是否超限
  assert_true(total_budget <= budget_limit)
  
  // 计算支付周期（每周期不超过10000）
  let payment_limit = 10000
  let payment_cycles = divide_with_ceil(total_budget, payment_limit)
  assert_eq(5, payment_cycles)  // ceil(48000/10000) = 5
  
  // 计算每周期平均支付金额
  let avg_payment = divide_with_ceil(total_budget, payment_cycles)
  assert_eq(9600, avg_payment)  // ceil(48000/5) = 9600
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_safe = 1000000
  let min_safe = -1000000
  
  // 大数运算测试
  assert_eq(2000000, add(max_safe, max_safe))
  assert_eq(0, add(max_safe, min_safe))
  assert_eq(-2000000, add(min_safe, min_safe))
  
  // 大数乘法测试
  assert_eq(1000000000000, multiply(max_safe, max_safe))
  assert_eq(-1000000000000, multiply(max_safe, min_safe))
  assert_eq(1000000000000, multiply(min_safe, min_safe))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(max_safe, max_safe))
  assert_eq(-1, divide_with_ceil(min_safe, max_safe))
  assert_eq(1, divide_with_ceil(min_safe, min_safe))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议座位安排
  let attendees = 87
  let tables_available = 12
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(2200, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(800, remaining_budget)
  
  // 计算座位利用率
  let utilization = multiply(divide_with_ceil(multiply(attendees, 100), total_seats), 1)
  assert_eq(98, utilization)  // floor(8700/88) = 98%
}

test "performance_and_optimization" {
  // 性能和优化测试
  // 大规模数据处理模拟
  let data_size = 10000
  let batch_size = 75
  let processing_time_per_batch = 3
  
  // 计算需要的处理批次
  let batches_needed = divide_with_ceil(data_size, batch_size)
  assert_eq(134, batches_needed)  // ceil(10000/75) = 134
  
  // 计算总处理时间
  let total_time = multiply(batches_needed, processing_time_per_batch)
  assert_eq(402, total_time)
  
  // 计算实际处理的数据量
  let full_batches = subtract(batches_needed, 1)
  let processed_in_full_batches = multiply(full_batches, batch_size)
  let final_batch_size = subtract(data_size, processed_in_full_batches)
  assert_eq(25, final_batch_size)  // 最后一批处理25个
  
  // 算法复杂度对比
  let linear_time = multiply(data_size, 2)
  let optimized_time = multiply(batches_needed, processing_time_per_batch)
  let improvement_ratio = divide_with_ceil(linear_time, optimized_time)
  assert_eq(50, improvement_ratio)  // ceil(20000/402) = 50
  
  // 内存使用优化计算
  let memory_per_batch = 100
  let total_memory_linear = multiply(data_size, memory_per_batch)
  let total_memory_batched = multiply(batches_needed, memory_per_batch)
  let memory_savings = subtract(total_memory_linear, total_memory_batched)
  assert_eq(986600, memory_savings)  // 1000000 - 13400 = 986600
}