// 新的综合 MoonBit 测试用例
// 包含8个高质量的标准 MoonBit 测试用例

test "arithmetic_sequence_operations" {
  // 算术序列操作测试
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的第n项: a_n = a_1 + (n-1)*d
  let term1 = start
  let term2 = azimuth.add(start, difference)
  let term3 = azimuth.add(term2, difference)
  let term4 = azimuth.add(term3, difference)
  
  // 计算数列和
  let sum = azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4)
  
  azimuth.assert_eq(5, term1)
  azimuth.assert_eq(8, term2)
  azimuth.assert_eq(11, term3)
  azimuth.assert_eq(14, term4)
  azimuth.assert_eq(38, sum)
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  let total_tasks = 125
  let team_capacity = 8
  let working_days = 5
  
  // 计算每天需要的团队数量
  let daily_tasks = azimuth.divide_with_ceil(total_tasks, working_days)
  azimuth.assert_eq(25, daily_tasks)
  
  // 计算需要的团队数量
  let teams_needed = azimuth.divide_with_ceil(daily_tasks, team_capacity)
  azimuth.assert_eq(4, teams_needed)
  
  // 验证总处理能力
  let total_capacity = azimuth.multiply(teams_needed, azimuth.multiply(team_capacity, working_days))
  azimuth.assert_eq(160, total_capacity)
  
  // 计算资源利用率
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(total_tasks, 100), total_capacity)
  azimuth.assert_eq(79, utilization)  // floor(12500/160) = 78%
}

test "string_concatenation_edge_cases" {
  // 字符串连接边缘情况测试
  azimuth.assert_eq_string("Hello, single!", azimuth.greet("single"))
  azimuth.assert_eq_string("Hello, multiple words!", azimuth.greet("multiple words"))
  azimuth.assert_eq_string("Hello, 123456!", azimuth.greet("123456"))
  azimuth.assert_eq_string("Hello, special@#$%!", azimuth.greet("special@#$%"))
  azimuth.assert_eq_string("Hello, mixedCASE123!", azimuth.greet("mixedCASE123"))
  azimuth.assert_eq_string("Hello,   spaces   !", azimuth.greet("   spaces   "))
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 第一年结束
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年结束
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年结束
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(800, year1_interest)
  assert_eq(10800, year1_total)
  assert_eq(864, year2_interest)
  assert_eq(11664, year2_total)
  assert_eq(934, year3_interest)
  assert_eq(12598, year3_total)
}

test "logistics_distribution_network" {
  // 物流配送网络测试
  let total_packages = 247
  let warehouse_capacity = 60
  let truck_capacity = 15
  
  // 计算需要的仓库数量
  let warehouses_needed = divide_with_ceil(total_packages, warehouse_capacity)
  assert_eq(5, warehouses_needed)
  
  // 计算需要的运输次数
  let trips_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(17, trips_needed)
  
  // 计算总存储容量
  let total_storage = multiply(warehouses_needed, warehouse_capacity)
  assert_eq(300, total_storage)
  
  // 计算剩余存储空间
  let remaining_storage = subtract(total_storage, total_packages)
  assert_eq(53, remaining_storage)
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 计算平方和: 1² + 2² + 3² + 4² + 5²
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  let square5 = multiply(5, 5)
  
  let sum_of_squares = add(add(add(add(square1, square2), square3), square4), square5)
  assert_eq(55, sum_of_squares)
  
  // 计算立方和: 1³ + 2³ + 3³
  let cube1 = multiply(multiply(1, 1), 1)
  let cube2 = multiply(multiply(2, 2), 2)
  let cube3 = multiply(multiply(3, 3), 3)
  
  let sum_of_cubes = add(add(cube1, cube2), cube3)
  assert_eq(36, sum_of_cubes)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 极值运算
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
}

test "complex_workflow_simulation" {
  // 复杂工作流模拟测试
  // 场景：项目管理中的资源分配和时间估算
  let project_tasks = 89
  let senior_developers = 3
  let junior_developers = 4
  let senior_velocity = 8
  let junior_velocity = 5
  let sprint_weeks = 2
  
  // 计算团队总处理能力
  let senior_capacity = multiply(senior_developers, senior_velocity)
  let junior_capacity = multiply(junior_developers, junior_velocity)
  let total_weekly_capacity = add(senior_capacity, junior_capacity)
  
  // 计算冲刺处理能力
  let sprint_capacity = multiply(total_weekly_capacity, sprint_weeks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(project_tasks, sprint_capacity)
  
  assert_eq(24, senior_capacity)
  assert_eq(20, junior_capacity)
  assert_eq(44, total_weekly_capacity)
  assert_eq(88, sprint_capacity)
  assert_eq(2, sprints_needed)
  
  // 计算总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(176, total_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(total_capacity, project_tasks)
  assert_eq(87, remaining_capacity)
}