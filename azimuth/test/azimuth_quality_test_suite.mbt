// Azimuth 项目的高质量 MoonBit 测试用例
// 包含9个精心设计的测试用例，覆盖各种功能和场景

test "basic_arithmetic_comprehensive" {
  // 综合测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(5, -6))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-30, multiply(5, -6))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边缘情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_properties_validation" {
  // 验证基本数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零性质
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "business_logistics_calculation" {
  // 业务物流计算测试：仓库装箱问题
  let total_items = 157
  let box_capacity = 24
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_size = 8
  let hours_per_day = 8
  let working_days = 15
  let hourly_rate = 50
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(48000, total_cost)
  
  // 计算每期预算（不超过10000）
  let budget_per_period = 10000
  let periods_needed = divide_with_ceil(total_cost, budget_per_period)
  assert_eq(5, periods_needed)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables_available = 10
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 如果有足够桌子，计算总座位数
  let total_seats = multiply(tables_available, chairs_per_table)
  assert_eq(80, total_seats)
  
  // 计算额外需要的桌子
  let additional_tables_needed = subtract(tables_needed, tables_available)
  assert_eq(1, additional_tables_needed)
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  // 模拟不同算法的时间复杂度
  let input_size = 1000
  
  // O(1) 恒定时间算法
  let constant_time = 5
  
  // O(n) 线性时间算法
  let linear_time = multiply(input_size, 2)
  assert_eq(2000, linear_time)
  
  // O(log n) 对数时间算法（简化模拟）
  let logarithmic_time = divide_with_ceil(input_size, 100)
  assert_eq(10, logarithmic_time)
  
  // O(n²) 平方时间算法（简化模拟）
  let quadratic_time = multiply(input_size, divide_with_ceil(input_size, 100))
  assert_eq(10000, quadratic_time)
  
  // 验证算法效率比较
  let comparison1 = constant_time < logarithmic_time
  let comparison2 = logarithmic_time < linear_time
  let comparison3 = linear_time < quadratic_time
  
  // 使用数值比较而不是布尔断言
  assert_eq(1, if (comparison1) { 1 } else { 0 })
  assert_eq(1, if (comparison2) { 1 } else { 0 })
  assert_eq(1, if (comparison3) { 1 } else { 0 })
}