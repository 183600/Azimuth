// Azimuth 增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "boundary_value_testing" {
  // 边界值测试：测试函数在极限条件下的行为
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "large_number_operations" {
  // 大数运算测试：验证函数在处理大数时的正确性
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(-1000000000000, multiply(-1000000, 1000000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(999001, divide_with_ceil(999000000, 1000))
}

test "mathematical_identities" {
  // 数学恒等式测试：验证基本的数学性质
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_logistics_scenario" {
  // 业务物流场景测试：模拟真实的物流计算需求
  let total_packages = 258
  let truck_capacity = 45
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(6, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(12, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(96, utilization_percent)  // floor(25800/270) = 95%
}

test "error_handling_comprehensive" {
  // 综合错误处理测试：验证函数在异常情况下的行为
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算的安全性
  let max_safe = 1000000
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(0, divide_with_ceil(0, max_safe))
}

test "string_special_input_handling" {
  // 字符串特殊输入处理测试：验证 greet 函数对特殊输入的处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, line\nbreak!", greet("line\nbreak"))
  assert_eq_string("Hello, tab\there!", greet("tab\there"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试：模拟多步骤的计算过程
  // 场景：项目资源分配计算
  let project_budget = 50000
  let team_members = 8
  let project_duration = 12  // 月
  
  // 计算每人每月预算
  let monthly_budget_per_person = divide_with_ceil(project_budget, multiply(team_members, project_duration))
  assert_eq(521, monthly_budget_per_person)  // ceil(50000/96) = 521
  
  // 计算总分配预算
  let total_allocated = multiply(monthly_budget_per_person, multiply(team_members, project_duration))
  assert_eq(50016, total_allocated)
  
  // 计算预算溢出
  let budget_overflow = subtract(total_allocated, project_budget)
  assert_eq(16, budget_overflow)
}

test "performance_scaling_test" {
  // 性能扩展性测试：验证函数在不同规模下的行为一致性
  let small_scale = 10
  let medium_scale = 100
  let large_scale = 1000
  
  // 验证加法的线性扩展
  assert_eq(20, add(small_scale, small_scale))
  assert_eq(200, add(medium_scale, medium_scale))
  assert_eq(2000, add(large_scale, large_scale))
  
  // 验证除法的一致性行为
  assert_eq(2, divide_with_ceil(multiply(small_scale, 3), small_scale))
  assert_eq(3, divide_with_ceil(multiply(medium_scale, 3), medium_scale))
  assert_eq(3, divide_with_ceil(multiply(large_scale, 3), large_scale))
  
  // 验证复杂计算的扩展性
  let small_result = divide_with_ceil(add(multiply(small_scale, 2), small_scale), 3)
  let medium_result = divide_with_ceil(add(multiply(medium_scale, 2), medium_scale), 3)
  let large_result = divide_with_ceil(add(multiply(large_scale, 2), large_scale), 3)
  
  assert_eq(10, small_result)   // ceil(30/3) = 10
  assert_eq(100, medium_result) // ceil(300/3) = 100
  assert_eq(1000, large_result) // ceil(3000/3) = 1000
}

test "data_type_boundary_testing" {
  // 数据类型边界测试：验证函数在数据类型边界附近的正确性
  // 测试接近 Int 类型极值的计算（使用安全的较大值）
  let large_positive = 1000000
  let large_negative = -1000000
  
  // 大数运算
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  assert_eq(1000000000000, multiply(large_positive, large_positive))
  assert_eq(1000000000000, multiply(large_negative, large_negative))
  assert_eq(-1000000000000, multiply(large_positive, large_negative))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(large_positive, large_positive))
  assert_eq(1, divide_with_ceil(large_negative, large_negative))
  assert_eq(-1, divide_with_ceil(large_positive, large_negative))
  assert_eq(-1, divide_with_ceil(large_negative, large_positive))
}

test "real_world_event_planning" {
  // 真实世界活动策划测试：模拟实际的活动策划计算场景
  let attendees = 387
  let tables_available = 50
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 10000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(49, tables_needed)
  
  // 验证桌子数量是否在可用范围内
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(392, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(5, remaining_seats)
  
  // 计算椅子总成本
  let total_chair_cost = multiply(total_seats, cost_per_chair)
  assert_eq(9800, total_chair_cost)
  
  // 验证预算是否充足
  assert_true(total_chair_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_chair_cost)
  assert_eq(200, remaining_budget)
  
  // 计算预算使用率
  let budget_usage_percent = divide_with_ceil(multiply(total_chair_cost, 100), total_budget)
  assert_eq(98, budget_usage_percent)  // floor(980000/10000) = 98%
}