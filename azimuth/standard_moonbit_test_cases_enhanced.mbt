// 标准 MoonBit 测试用例 - 增强版
// 为 azimuth 库添加的高质量测试用例

test "arithmetic_identity_verification" {
  // 算术恒等式验证测试
  let x = 42
  
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

test "divide_with_ceil_precision_scenarios" {
  // 向上取整除法精度场景测试
  // 测试各种精度情况下的向上取整行为
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  
  // 测试大数精度
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(667, divide_with_ceil(2001, 3))
  assert_eq(501, divide_with_ceil(2001, 4))
}

test "string_formatting_comprehensive" {
  // 字符串格式化综合测试
  assert_eq_string("Hello, Developer!", greet("Developer"))
  assert_eq_string("Hello, MoonBit 2024!", greet("MoonBit 2024"))
  assert_eq_string("Hello, @user123!", greet("@user123"))
  assert_eq_string("Hello, $100!", greet("$100"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello, spaces around !", greet("spaces around "))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算运输需求
  let total_weight = 1250  // 总重量(kg)
  let truck_capacity = 300  // 卡车容量(kg)
  
  let trucks_needed = divide_with_ceil(total_weight, truck_capacity)
  assert_eq(5, trucks_needed)  // ceil(1250/300) = 5
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(1500, total_capacity)
  
  // 计算剩余运力
  let remaining_capacity = subtract(total_capacity, total_weight)
  assert_eq(250, remaining_capacity)
}

test "negative_number_comprehensive" {
  // 负数综合处理测试
  // 加法负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 乘法负数测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 减法负数测试
  assert_eq(-10, subtract(5, 15))
  assert_eq(20, subtract(5, -15))
  assert_eq(-20, subtract(-5, 15))
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 1000
  let departments = 7
  
  // 计算每个部门的最小资源分配
  let min_allocation = divide_with_ceil(total_resources, departments)
  assert_eq(143, min_allocation)  // ceil(1000/7) = 143
  
  // 验证总分配量
  let total_allocated = multiply(min_allocation, departments)
  assert_eq(1001, total_allocated)
  
  // 计算资源利用率
  let utilization_percent = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(99, utilization_percent)  // floor(100000/1001) = 99%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目成本估算
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(57600, total_cost)  // 768 * 75 = 57600
  
  // 计算付款期数（每期不超过10000）
  let payment_limit = 10000
  let payment_periods = divide_with_ceil(total_cost, payment_limit)
  assert_eq(6, payment_periods)  // ceil(57600/10000) = 6
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的全面处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let max_safe = 1000000
  let min_safe = -1000000
  
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(0, multiply(min_safe, 0))
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(min_safe, add(min_safe, 0))
}

test "mathematical_sequence_validation" {
  // 数学序列验证测试
  // 斐波那契式序列计算
  let a = 1
  let b = 1
  let c = add(a, b)  // 2
  let d = add(b, c)  // 3
  let e = add(c, d)  // 5
  let f = add(d, e)  // 8
  
  assert_eq(2, c)
  assert_eq(3, d)
  assert_eq(5, e)
  assert_eq(8, f)
  
  // 验证黄金比例近似
  let ratio_approx = divide_with_ceil(multiply(f, 100), e)
  assert_eq(160, ratio_approx)  // ceil(8*100/5) = 160 (1.6的近似)
}

test "real_world_event_planning_comprehensive" {
  // 真实世界活动规划综合测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 10
  let cost_per_chair = 25
  let budget_limit = 8000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(26, tables_needed)  // ceil(256/10) = 26
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(260, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(4, empty_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6500, total_cost)
  
  // 计算剩余预算
  let remaining_budget = subtract(budget_limit, total_cost)
  assert_eq(1500, remaining_budget)
}