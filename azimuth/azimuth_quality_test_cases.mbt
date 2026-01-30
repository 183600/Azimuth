// Azimuth 质量测试用例 - 标准 MoonBit 测试语法
// 包含10个高质量测试用例，覆盖各种功能和场景

test "basic_arithmetic_commutativity" {
  // 测试基本算术运算的交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  
  // 测试大数精度
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(667, divide_with_ceil(2001, 3))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 123
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_packages = 157
  let vehicles_available = 8
  let capacity_per_vehicle = 20
  
  // 计算需要的车辆数量
  let vehicles_needed = divide_with_ceil(total_packages, capacity_per_vehicle)
  assert_eq(8, vehicles_needed)  // ceil(157/20) = 8
  
  // 检查车辆是否足够
  assert_true(vehicles_needed <= vehicles_available)
  
  // 计算总运力
  let total_capacity = multiply(vehicles_needed, capacity_per_vehicle)
  assert_eq(160, total_capacity)
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(3, remaining_capacity)
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  // 加法测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 乘法测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 除法测试
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(-1, divide_with_ceil(5, -3))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 12
  let project_duration_days = 15
  let daily_working_hours = 8
  let hourly_cost = 50
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(project_duration_days, daily_working_hours))
  assert_eq(1440, total_hours)  // 12 * 15 * 8 = 1440
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_cost)
  assert_eq(72000, total_cost)  // 1440 * 50 = 72000
  
  // 计算每期支付金额（分10期支付）
  let payment_periods = 10
  let payment_per_period = divide_with_ceil(total_cost, payment_periods)
  assert_eq(7200, payment_per_period)  // ceil(72000/10) = 7200
  
  // 验证总支付金额
  let total_payment = multiply(payment_per_period, payment_periods)
  assert_eq(72000, total_payment)
}

test "string_handling_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, space between!", greet("space between"))
  assert_eq_string("Hello, MixedCASE123!", greet("MixedCASE123"))
}

test "error_handling_boundary_conditions" {
  // 错误处理边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 250
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(250/8) = 32
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算（这里总成本超过预算，我们记录这个情况）
  let budget_exceeded = total_cost > total_budget
  assert_true(budget_exceeded)
  
  // 计算预算差额
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(400, budget_shortage)
}