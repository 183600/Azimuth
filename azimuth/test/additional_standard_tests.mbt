// 额外的标准 MoonBit 测试用例 - 10个高质量测试用例

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(-500000, 500000))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(0, @azimuth.multiply(1000000, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1000000))
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, mixedCASE!", @azimuth.greet("mixedCASE"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "mathematical_distributive_property" {
  // 测试数学分配律性质
  let a = 6
  let b = 4
  let c = 3
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  @azimuth.assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let initial = 5
  let step1 = @azimuth.add(initial, @azimuth.multiply(2, 3))     // 5 + 2*3 = 11
  let step2 = @azimuth.divide_with_ceil(step1, 4)               // ceil(11/4) = 3
  let step3 = @azimuth.multiply(step2, @azimuth.add(1, 2))       // 3 * (1+2) = 9
  let step4 = @azimuth.divide_with_ceil(step3, 5)               // ceil(9/5) = 2
  @azimuth.assert_eq(2, step4)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  @azimuth.assert_eq(34, min_resources_per_team)  // ceil(100/3) = 34
  
  // 验证分配结果
  let allocated_total = @azimuth.multiply(min_resources_per_team, teams)
  @azimuth.assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 100), allocated_total)
  @azimuth.assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "error_handling_boundary_cases" {
  // 测试错误处理边界情况
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值验证
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "real_world_application_test" {
  // 测试真实世界应用场景：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(available_seats, participants)
  @azimuth.assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  @azimuth.assert_true(available_seats >= participants)
}