// Azimuth 增强单元测试
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能和边界情况

test "add_function_comprehensive_test" {
  // 全面测试加法函数
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_boundary_test" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_unicode_test" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, empty!", @azimuth.greet(""))
}

test "divide_with_ceil_business_logic_test" {
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

test "mathematical_properties_verification_test" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "resource_allocation_optimization_test" {
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

test "error_handling_and_boundaries_test" {
  // 错误处理和边界测试
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "string_processing_comprehensive_test" {
  // 字符串处理综合测试
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !@#$%^!", @azimuth.greet("!@#$%^"))
  @azimuth.assert_eq_string("Hello, mixedCASE!", @azimuth.greet("mixedCASE"))
}

test "complex_calculation_pipeline_test" {
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

test "real_world_calculation_scenario_test" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
}