// 标准 MoonBit 测试用例
// 为 azimuth 库提供高质量测试覆盖

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(50, -50))
  assert_eq(999, add(1, 998))
  assert_eq(1, add(-2, 3))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(0, multiply(12345, 0))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, Tokyo!", greet("Tokyo"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, Cairo!", greet("Cairo"))
}

test "divide_with_ceil_real_world_applications" {
  // 测试向上取整除法的真实世界应用
  // 场景：计算需要的房间数量
  let guests = 35
  let capacity_per_room = 4
  let rooms_needed = divide_with_ceil(guests, capacity_per_room)
  assert_eq(9, rooms_needed)
  
  // 验证总容量
  let total_capacity = multiply(rooms_needed, capacity_per_room)
  assert_eq(36, total_capacity)
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 场景：计算项目成本和时间
  let team_size = 6
  let daily_cost_per_person = 200
  let project_duration_days = 15
  
  // 计算总成本
  let daily_team_cost = multiply(team_size, daily_cost_per_person)
  let total_project_cost = multiply(daily_team_cost, project_duration_days)
  assert_eq(18000, total_project_cost)
  
  // 计算需要的支付周期（每周期支付不超过5000）
  let payment_cycle_limit = 5000
  let payment_cycles = divide_with_ceil(total_project_cost, payment_cycle_limit)
  assert_eq(4, payment_cycles)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_cases_and_boundary_conditions" {
  // 测试边缘情况和边界条件
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 单位值测试
  assert_eq(12345, add(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(1, divide_with_ceil(12345, 12345))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC123!", greet("ABC123"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "resource_optimization_scenario" {
  // 资源优化场景测试
  // 场景：仓库存储优化
  let total_items = 234
  let box_types = 3
  let items_per_box_type = divide_with_ceil(total_items, box_types)
  assert_eq(78, items_per_box_type)
  
  // 验证每种类型箱子的总容量
  let total_capacity = multiply(items_per_box_type, box_types)
  assert_eq(234, total_capacity)
  
  // 计算剩余空间
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(0, unused_space)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 1000
  
  // 模拟O(n)算法
  let linear_complexity = multiply(input_size, 3)
  assert_eq(3000, linear_complexity)
  
  // 模拟O(log n)算法（简化为除法）
  let logarithmic_complexity = divide_with_ceil(input_size, 10)
  assert_eq(100, logarithmic_complexity)
  
  // 模拟O(n²)算法（简化为乘法）
  let quadratic_complexity = multiply(input_size, divide_with_ceil(input_size, 100))
  assert_eq(10000, quadratic_complexity)
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}