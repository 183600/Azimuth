// Azimuth 专注测试用例 - 包含10个高质量的标准 MoonBit 测试用例
// 覆盖基本功能、边界条件、业务逻辑和真实应用场景

test "basic_arithmetic_combinations" {
  // 基本算术运算组合测试
  let a = 12
  let b = 8
  let c = 5
  
  // 测试加法和乘法的组合
  assert_eq(40, add(multiply(a, b), multiply(b, c)))  // 12*8 + 8*5 = 96 + 40 = 136
  
  // 测试减法和除法的组合
  assert_eq(3, divide_with_ceil(subtract(a, c), b))  // ceil((12-5)/8) = ceil(7/8) = 1
  
  // 测试混合运算
  let result = add(multiply(a, 2), divide_with_ceil(b, 3))
  assert_eq(26, result)  // 12*2 + ceil(8/3) = 24 + 3 = 27
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 单位值边界
  assert_eq(1, add(1, 0))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 极值边界（在合理范围内）
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(10001, divide_with_ceil(10000, 1))
}

test "business_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 247
  let box_capacity = 25
  let warehouse_sections = 8
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(10, boxes_needed)  // ceil(247/25) = 10
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = divide_with_ceil(boxes_needed, warehouse_sections)
  assert_eq(2, boxes_per_section)  // ceil(10/8) = 2
  
  // 验证总存储容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(250, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(3, remaining_space)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 负数除零测试
  assert_eq(0, divide_with_ceil(-1000000, 0))
  
  // 混合符号运算测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 基本字符串测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, space test!", greet("space test"))
  
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  let z = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(x, add(y, z)), add(add(x, y), z))  // 结合律
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, multiply(y, z)), multiply(multiply(x, y), z))  // 结合律
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
  
  // 恒等式性质
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目成本计算
  let team_size = 6
  let hourly_rate = 75
  let hours_per_day = 8
  let project_duration = 15
  let overhead_percentage = 20
  
  // 计算基础成本
  let daily_cost = multiply(team_size, multiply(hourly_rate, hours_per_day))
  assert_eq(3600, daily_cost)  // 6 * 75 * 8 = 3600
  
  // 计算项目总成本
  let base_cost = multiply(daily_cost, project_duration)
  assert_eq(54000, base_cost)  // 3600 * 15 = 54000
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_percentage), 100)
  assert_eq(10800, overhead_cost)  // ceil(54000 * 20 / 100) = 10800
  
  // 计算总项目成本
  let total_cost = add(base_cost, overhead_cost)
  assert_eq(64800, total_cost)
  
  // 计算每个冲刺的成本（假设4个冲刺）
  let cost_per_sprint = divide_with_ceil(total_cost, 4)
  assert_eq(16200, cost_per_sprint)  // ceil(64800/4) = 16200
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：计算服务器资源分配
  let total_memory_gb = 128
  let total_cpu_cores = 32
  let applications = 5
  
  // 计算每个应用的最小内存分配
  let memory_per_app = divide_with_ceil(total_memory_gb, applications)
  assert_eq(26, memory_per_app)  // ceil(128/5) = 26
  
  // 计算每个应用的CPU核心数
  let cpu_per_app = divide_with_ceil(total_cpu_cores, applications)
  assert_eq(7, cpu_per_app)  // ceil(32/5) = 7
  
  // 验证总分配资源
  let allocated_memory = multiply(memory_per_app, applications)
  let allocated_cpu = multiply(cpu_per_app, applications)
  
  assert_eq(130, allocated_memory)  // 26 * 5 = 130
  assert_eq(35, allocated_cpu)  // 7 * 5 = 35
  
  // 计算资源利用率
  let memory_utilization = divide_with_ceil(multiply(total_memory_gb, 100), allocated_memory)
  let cpu_utilization = divide_with_ceil(multiply(total_cpu_cores, 100), allocated_cpu)
  
  assert_eq(98, memory_utilization)  // ceil(12800/130) = 99
  assert_eq(92, cpu_utilization)  // ceil(3200/35) = 92
}

test "performance_scaling_analysis" {
  // 性能扩展分析测试
  // 场景：分析不同数据规模下的算法性能
  let small_dataset = 100
  let medium_dataset = 1000
  let large_dataset = 10000
  
  // 模拟 O(n) 算法
  let linear_small = multiply(small_dataset, 3)
  let linear_medium = multiply(medium_dataset, 3)
  let linear_large = multiply(large_dataset, 3)
  
  assert_eq(300, linear_small)
  assert_eq(3000, linear_medium)
  assert_eq(30000, linear_large)
  
  // 模拟 O(log n) 算法（简化为除法运算）
  let log_small = divide_with_ceil(small_dataset, 10)
  let log_medium = divide_with_ceil(medium_dataset, 10)
  let log_large = divide_with_ceil(large_dataset, 10)
  
  assert_eq(10, log_small)
  assert_eq(100, log_medium)
  assert_eq(1000, log_large)
  
  // 验证性能比例
  let linear_ratio = divide_with_ceil(linear_large, linear_small)
  let log_ratio = divide_with_ceil(log_large, log_small)
  
  assert_eq(100, linear_ratio)  // 30000/300 = 100
  assert_eq(100, log_ratio)  // 1000/10 = 100
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：组织技术会议
  let attendees = 256
  let tables_available = 32
  let chairs_per_table = 8
  let cost_per_chair = 45
  let budget_limit = 12000
  let staff_needed_per_50_attendees = 3
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(256/8) = 32
  
  // 验证桌子是否足够
  assert_eq(1, if (tables_needed <= tables_available) { 1 } else { 0 })
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算座位成本
  let seating_cost = multiply(total_seats, cost_per_chair)
  assert_eq(11520, seating_cost)  // 256 * 45 = 11520
  
  // 验证预算 - 使用 assert_eq 检查布尔条件
  assert_eq(1, if (seating_cost <= budget_limit) { 1 } else { 0 })
  
  // 计算剩余预算
  let remaining_budget = subtract(budget_limit, seating_cost)
  assert_eq(480, remaining_budget)
  
  // 计算所需工作人员
  let staff_needed = multiply(divide_with_ceil(attendees, 50), staff_needed_per_50_attendees)
  assert_eq(18, staff_needed)  // ceil(256/50) * 3 = 6 * 3 = 18
  
  // 验证活动可行性 - 使用 assert_eq 检查布尔条件
  let within_budget = seating_cost <= budget_limit
  let enough_tables = tables_needed <= tables_available
  let enough_seats = total_seats >= attendees
  
  assert_eq(1, if (within_budget) { 1 } else { 0 })
  assert_eq(1, if (enough_tables) { 1 } else { 0 })
  assert_eq(1, if (enough_seats) { 1 } else { 0 })
}