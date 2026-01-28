// Azimuth 综合测试套件
// 包含10个高质量的标准 MoonBit 测试用例

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "inventory_management_calculation" {
  // 库存管理计算测试
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(3, boxes_a)  // ceil(45/20) = 3
  assert_eq(2, boxes_b)  // ceil(32/20) = 2
  assert_eq(4, boxes_c)  // ceil(67/20) = 4
  assert_eq(9, total_boxes)  // 3 + 2 + 4 = 9
}

test "financial_loan_calculation" {
  // 金融贷款计算测试
  let loan_amount = 50000
  let monthly_payment = 2200
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算实际总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)  // 23 * 2200 = 50600
  
  // 计算最后一个月的实际支付金额
  let previous_payments = multiply(months_needed - 1, monthly_payment)
  let final_payment = subtract(total_payment, previous_payments)
  assert_eq(200, final_payment)  // 50600 - 22*2200 = 200
}

test "complex_sequential_operations" {
  // 复杂连续操作测试
  let initial = 15
  
  // 连续操作链
  let step1 = multiply(initial, 3)        // 45
  let step2 = add(step1, 10)              // 55
  let step3 = divide_with_ceil(step2, 8)  // 7
  let step4 = multiply(step3, 2)          // 14
  let final_result = subtract(step4, 5)   // 9
  
  assert_eq(9, final_result)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 测试大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "unicode_and_special_character_processing" {
  // Unicode和特殊字符处理测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, 🌟🌙⭐!", greet("🌟🌙⭐"))
  assert_eq_string("Hello, 
	!", greet("
	"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：服务器资源分配优化
  let total_cpu_cores = 64
  let total_memory_gb = 256
  let services = 5
  
  // 计算每个服务的最小资源分配
  let cpu_per_service = divide_with_ceil(total_cpu_cores, services)
  let memory_per_service = divide_with_ceil(total_memory_gb, services)
  
  assert_eq(13, cpu_per_service)     // ceil(64/5) = 13
  assert_eq(52, memory_per_service)  // ceil(256/5) = 52
  
  // 计算实际分配的总资源
  let total_allocated_cpu = multiply(cpu_per_service, services)
  let total_allocated_memory = multiply(memory_per_service, services)
  
  assert_eq(65, total_allocated_cpu)     // 13 * 5 = 65
  assert_eq(260, total_allocated_memory) // 52 * 5 = 260
  
  // 计算资源浪费
  let cpu_waste = subtract(total_allocated_cpu, total_cpu_cores)
  let memory_waste = subtract(total_allocated_memory, total_memory_gb)
  
  assert_eq(1, cpu_waste)     // 65 - 64 = 1
  assert_eq(4, memory_waste)  // 260 - 256 = 4
}

test "mathematical_associative_properties" {
  // 数学结合律性质测试
  let a = 4
  let b = 6
  let c = 8
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let add_left = add(add(a, b), c)
  let add_right = add(a, add(b, c))
  assert_eq(add_left, add_right)  // (4+6)+8 = 4+(6+8) = 18
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let multiply_left = multiply(multiply(a, b), c)
  let multiply_right = multiply(a, multiply(b, c))
  assert_eq(multiply_left, multiply_right)  // (4*6)*8 = 4*(6*8) = 192
}

test "performance_and_scalability_test" {
  // 性能和可扩展性测试
  // 测试不同规模的数据处理
  let small_dataset = 100
  let medium_dataset = 1000
  let large_dataset = 10000
  
  // 模拟O(n)算法的执行步数
  let small_steps = multiply(small_dataset, 3)
  let medium_steps = multiply(medium_dataset, 3)
  let large_steps = multiply(large_dataset, 3)
  
  assert_eq(300, small_steps)    // 100 * 3
  assert_eq(3000, medium_steps)  // 1000 * 3
  assert_eq(30000, large_steps)  // 10000 * 3
  
  // 模拟O(log n)算法的执行步数（简化为除法）
  let small_log_steps = divide_with_ceil(small_dataset, 10)
  let medium_log_steps = divide_with_ceil(medium_dataset, 10)
  let large_log_steps = divide_with_ceil(large_dataset, 10)
  
  assert_eq(10, small_log_steps)    // ceil(100/10)
  assert_eq(100, medium_log_steps)  // ceil(1000/10)
  assert_eq(1000, large_log_steps)  // ceil(10000/10)
  
  // 验证算法效率差异
  let small_efficiency_ratio = divide_with_ceil(small_steps, small_log_steps)
  let large_efficiency_ratio = divide_with_ceil(large_steps, large_log_steps)
  
  assert_eq(30, small_efficiency_ratio)  // 300/10 = 30
  assert_eq(30, large_efficiency_ratio)  // 30000/1000 = 30
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议和活动安排
  let participants = 156
  let tables_available = 12
  let chairs_per_table = 10
  let sessions_per_day = 4
  let conference_days = 3
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)  // ceil(156/10) = 16
  
  // 检查桌子是否足够
  let enough_tables = tables_available >= tables_needed
  assert_false(enough_tables)  // 12 < 16，桌子不够
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)  // 16 * 10 = 160
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(4, remaining_seats)  // 160 - 156 = 4
  
  // 计算总会议时段
  let total_sessions = multiply(sessions_per_day, conference_days)
  assert_eq(12, total_sessions)  // 4 * 3 = 12
  
  // 计算每个时段的平均参与人数
  let avg_participants_per_session = divide_with_ceil(participants, total_sessions)
  assert_eq(13, avg_participants_per_session)  // ceil(156/12) = 13
}