// 标准 MoonBit 测试用例 - Azimuth 项目增强测试
// 包含10个高质量测试用例，覆盖各种场景

test "arithmetic_identity_properties" {
  // 算术恒等式性质测试
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

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数场景
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(5, divide_with_ceil(21, 4))
  assert_eq(6, divide_with_ceil(22, 4))
  
  // 负数场景
  assert_eq(-5, divide_with_ceil(-20, 4))
  assert_eq(-6, divide_with_ceil(-21, 4))
  assert_eq(-6, divide_with_ceil(-22, 4))
  
  // 混合符号场景
  assert_eq(-5, divide_with_ceil(20, -4))
  assert_eq(5, divide_with_ceil(-20, -4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_internationalization" {
  // 字符串问候国际化测试
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_commutative_and_associative" {
  // 数学交换律和结合律测试
  let a = 7
  let b = 11
  let c = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(a, add(b, c)), add(add(a, b), c))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, multiply(b, c)), multiply(multiply(a, b), c))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_logistics_optimization" {
  // 业务物流优化测试
  let total_orders = 157
  let truck_capacity = 24
  let trucks_needed = divide_with_ceil(total_orders, truck_capacity)
  
  assert_eq(7, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余运力
  let remaining_capacity = subtract(total_capacity, total_orders)
  assert_eq(11, remaining_capacity)
  
  // 计算运力利用率
  let utilization_percent = divide_with_ceil(multiply(total_orders, 100), total_capacity)
  assert_eq(94, utilization_percent)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1920, total_tasks)
  
  // 计算需要的冲刺数（每冲刺处理500个任务）
  let tasks_per_sprint = 500
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(4, sprints_needed)
  
  // 计算实际处理能力
  let actual_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(2000, actual_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(actual_capacity, total_tasks)
  assert_eq(80, remaining_capacity)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算（使用较小的数避免溢出）
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(10001, divide_with_ceil(add(large_num, 1), large_num))
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 大数运算测试
  let base = 10000
  let multiplier = 1000
  let result = multiply(base, multiplier)
  assert_eq(10000000, result)
  
  // 复杂数学运算
  let complex_result = add(multiply(100, 200), divide_with_ceil(999, 10))
  assert_eq(20099, complex_result)
  
  // 连续运算链
  let chain_result = divide_with_ceil(
    multiply(
      add(10, 5),
      subtract(20, 8)
    ),
    7
  )
  assert_eq(26, chain_result)  // ceil((10+5)*(20-8)/7) = ceil(15*12/7) = ceil(180/7) = 26
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
  assert_eq(32, tables_needed)
  
  // 检查桌子是否足够（使用减法代替比较操作符）
  let table_difference = subtract(tables_available, tables_needed)
  assert_true(table_difference >= 0)  // tables_needed <= tables_available
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算（这里会超出预算，但测试计算逻辑）
  let budget_difference = subtract(total_cost, total_budget)
  assert_eq(400, budget_difference)
}

test "data_validation_and_integrity" {
  // 数据验证和完整性测试
  // 验证数学运算的一致性
  let test1_a = 1
  let test1_b = 2
  let test2_a = 10
  let test2_b = 5
  let test3_a = 100
  let test3_b = 25
  let test4_a = 7
  let test4_b = 3
  let test5_a = 50
  let test5_b = 50
  
  // 对每对值进行一致性验证
  assert_eq(3, add(test1_a, test1_b))
  assert_eq(50, multiply(test2_a, test2_b))
  assert_eq(4, divide_with_ceil(test3_a, test3_b))
  assert_eq(3, divide_with_ceil(test4_a, test4_b))
  assert_eq(1, divide_with_ceil(test5_a, test5_b))
  
  // 验证字符串操作的一致性
  let test_string1 = ""
  let test_string2 = "a"
  let test_string3 = "hello"
  let test_string4 = "123"
  let test_string5 = "测试"
  
  assert_eq_string("Hello, !", greet(test_string1))
  assert_eq_string("Hello, a!", greet(test_string2))
  assert_eq_string("Hello, hello!", greet(test_string3))
  assert_eq_string("Hello, 123!", greet(test_string4))
  assert_eq_string("Hello, 测试!", greet(test_string5))
}