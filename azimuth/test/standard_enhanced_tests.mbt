// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加的高质量测试
// 包含10个精心设计的测试用例，覆盖各种场景

test "comprehensive_arithmetic_validation" {
  // 综合算术验证测试
  // 测试基本算术运算的正确性
  assert_eq(42, add(20, 22))
  assert_eq(-5, add(10, -15))
  assert_eq(0, add(-100, 100))
  
  assert_eq(100, multiply(10, 10))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(100, multiply(-10, -10))
  assert_eq(0, multiply(1000, 0))
  
  assert_eq(10, subtract(30, 20))
  assert_eq(-10, subtract(20, 30))
  assert_eq(0, subtract(50, 50))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数除法
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(4, divide_with_ceil(16, 4))
  assert_eq(5, divide_with_ceil(17, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(25, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-4, divide_with_ceil(-13, 4))
  assert_eq(-3, divide_with_ceil(-12, 4))
  assert_eq(3, divide_with_ceil(-11, -4))
  assert_eq(4, divide_with_ceil(-13, -4))
}

test "greet_function_international_support" {
  // 问候函数的国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, user123!", greet("user123"))
}

test "mathematical_properties_and_identities" {
  // 数学性质和恒等式测试
  let x = 17
  let y = 23
  
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
  assert_eq(0, multiply(x, 0))
  
  // 减法逆运算
  assert_eq(x, add(subtract(x, y), y))
}

test "business_inventory_management_scenario" {
  // 库存管理业务场景测试
  let total_products = 258
  let box_capacity = 36
  let cost_per_box = 15
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(8, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(288, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(30, remaining_space)
  
  // 计算总成本
  let total_cost = multiply(boxes_needed, cost_per_box)
  assert_eq(120, total_cost)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划和成本计算
  let team_members = 12
  let hourly_rate = 45
  let hours_per_day = 8
  let project_days = 15
  let budget_limit = 50000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(1440, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(64800, total_cost)
  
  // 计算需要的预算周期（每周期不超过预算限制）
  let budget_cycles = divide_with_ceil(total_cost, budget_limit)
  assert_eq(2, budget_cycles)
  
  // 验证超预算金额
  let over_budget = subtract(total_cost, budget_limit)
  assert_eq(14800, over_budget)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极值测试
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 特殊字符串测试
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  let input_size = 1000
  
  // O(1) 常数时间算法
  let constant_time = 5
  assert_eq(5, constant_time)
  
  // O(n) 线性时间算法模拟
  let linear_time = multiply(input_size, 2)
  assert_eq(2000, linear_time)
  
  // O(log n) 对数时间算法模拟
  let log_time = divide_with_ceil(input_size, 100)
  assert_eq(10, log_time)
  
  // O(n²) 平方时间算法模拟
  let quadratic_time = multiply(input_size, divide_with_ceil(input_size, 100))
  assert_eq(10000, quadratic_time)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 156
  let tables_available = 20
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 验证桌子是否足够
  let chairs_available = multiply(tables_available, chairs_per_table)
  assert_eq(160, chairs_available)
  assert_true(chairs_available >= attendees)
  
  // 计算总成本
  let total_chairs_needed = multiply(tables_needed, chairs_per_table)
  let total_cost = multiply(total_chairs_needed, cost_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算
  assert_true(total_cost <= total_budget)
  
  // 计算剩余座位
  let empty_chairs = subtract(chairs_available, attendees)
  assert_eq(4, empty_chairs)
}

test "data_analysis_and_statistics" {
  // 数据分析和统计测试
  let data_points = 1000
  let sample_size = 75
  let confidence_level = 95
  
  // 计算样本数量（向上取整）
  let samples_needed = divide_with_ceil(data_points, sample_size)
  assert_eq(14, samples_needed)
  
  // 计算总样本数
  let total_samples = multiply(samples_needed, sample_size)
  assert_eq(1050, total_samples)
  
  // 计算额外样本
  let extra_samples = subtract(total_samples, data_points)
  assert_eq(50, extra_samples)
  
  // 计算置信区间的样本大小
  let confidence_samples = divide_with_ceil(multiply(data_points, confidence_level), 100)
  assert_eq(950, confidence_samples)
  
  // 验证样本覆盖率
  let coverage = multiply(divide_with_ceil(confidence_samples, data_points), 100)
  assert_eq(95, coverage)
}
