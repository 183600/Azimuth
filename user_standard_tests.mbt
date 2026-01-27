// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个高质量的测试用例，覆盖所有主要功能

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_packaging_scenario" {
  // 测试向上取整除法的包装场景
  // 场景：25个物品，每箱装6个，需要多少箱
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(5, boxes)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes, capacity)
  assert_eq(30, total_capacity)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(-5, -20))
  assert_eq(100, subtract(50, -50))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：项目资源规划
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  
  // 计算需要的支付周期（每周期不超过10000）
  let payment_limit = 10000
  let payment_cycles = divide_with_ceil(total_cost, payment_limit)
  
  assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  assert_eq(57600, total_cost)  // 768 * 75 = 57600
  assert_eq(6, payment_cycles)  // ceil(57600/10000) = 6
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 减法性质
  assert_eq(x, add(subtract(x, y), y))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：服务器内存分配
  let total_memory = 32768  // GB
  let servers = 7
  
  // 计算每台服务器应分配的内存
  let memory_per_server = divide_with_ceil(total_memory, servers)
  assert_eq(4682, memory_per_server)  // ceil(32768/7) = 4682
  
  // 计算实际分配的总内存
  let allocated_total = multiply(memory_per_server, servers)
  assert_eq(32774, allocated_total)
  
  // 计算内存浪费
  let memory_waste = subtract(allocated_total, total_memory)
  assert_eq(6, memory_waste)
}

test "real_world_event_planning" {
  // 测试真实世界活动规划场景
  // 场景：会议安排
  let participants = 156
  let tables = 12
  let chairs_per_table = 15
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(156/15) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(165, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(9, remaining_seats)
  
  // 验证资源充足性
  assert_true(total_seats >= participants)
  
  // 计算每桌平均人数
  let avg_per_table = divide_with_ceil(participants, tables_needed)
  assert_eq(15, avg_per_table)  // ceil(156/11) = 15
}