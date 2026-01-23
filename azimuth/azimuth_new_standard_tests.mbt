// 标准 MoonBit 测试用例 - 新增10个高质量测试
// 遵循标准 MoonBit 测试语法

// 测试加法函数的基本功能和边界情况
test "add_function_comprehensive" {
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

// 测试乘法函数的数学性质
test "multiply_function_properties" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

// 测试问候函数的Unicode和特殊字符支持
test "greet_function_unicode_support" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 测试向上取整除法的实际业务场景
test "divide_with_ceil_business_logic" {
  // 场景：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = @azimuth.multiply(trucks_needed, capacity_per_truck)
  @azimuth.assert_eq(50, total_capacity)
}

// 测试复杂计算流水线
test "complex_calculation_pipeline" {
  // 项目资源规划场景
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  @azimuth.assert_eq(3, sprints_needed)
  
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(45, total_capacity)
}

// 测试数学交换律和结合律
test "mathematical_laws_verification" {
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

// 测试负数和零值的边缘情况
test "negative_and_zero_edge_cases" {
  // 加法零值测试
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

// 测试资源分配优化问题
test "resource_allocation_optimization" {
  let total_budget = 10000
  let team_members = 6
  
  let max_share = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(1667, max_share)
  
  let total_allocated = @azimuth.multiply(max_share, team_members)
  @azimuth.assert_eq(10002, total_allocated)
}

// 测试错误处理和边界条件
test "error_handling_boundaries" {
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

// 测试真实世界计算场景
test "real_world_calculation_scenario" {
  // 活动座位安排场景
  let participants = 87
  let chairs_per_table = 8
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 验证座位数足够
  @azimuth.assert_true(total_seats >= participants)
}