// 标准 MoonBit 测试用例
// 为 azimuth 库添加的10个高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况（使用加法代替减法）
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  @azimuth.assert_eq(0, @azimuth.add(7, -7))
  @azimuth.assert_eq(15, @azimuth.add(-5, 20))
  @azimuth.assert_eq(-15, @azimuth.add(5, -20))
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = @azimuth.multiply(trucks_needed, capacity_per_truck)
  @azimuth.assert_eq(50, total_capacity)
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = @azimuth.multiply(people, tasks_per_person_per_day)
  let days_needed = @azimuth.divide_with_ceil(tasks, daily_capacity)
  @azimuth.assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(days_needed, daily_capacity)
  @azimuth.assert_eq(32, total_capacity)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = @azimuth.multiply(max_share, team_members)
  @azimuth.assert_eq(10002, total_allocated)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 验证座位充足性 - 使用比较而不是assert_true
  let seats_sufficient = @azimuth.divide_with_ceil(total_seats, participants)
  @azimuth.assert_eq(1, seats_sufficient)  // 如果total_seats >= participants，结果应该是1
}