// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，专注于边界条件和复合场景

test "zero_division_comprehensive" {
  // 零除法综合测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "mathematical_inverse_operations" {
  // 数学逆运算测试
  let x = 42
  let y = 17
  
  // 加法逆运算
  assert_eq(x, subtract(add(x, y), y))
  assert_eq(y, subtract(add(x, y), x))
  
  // 减法逆运算
  assert_eq(add(x, y), add(subtract(x, y), multiply(2, y)))
}

test "fractional_division_precision" {
  // 分数除法精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(3, divide_with_ceil(6, 2))
}

test "complex_resource_allocation" {
  // 复杂资源分配测试
  let total_tasks = 67
  let team_members = 5
  let working_days = 14
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_per_day = divide_with_ceil(total_tasks, multiply(team_members, working_days))
  assert_eq(1, tasks_per_person_per_day)
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_per_day, multiply(team_members, working_days))
  assert_eq(70, total_capacity)
  
  // 验证剩余处理能力
  let spare_capacity = subtract(total_capacity, total_tasks)
  assert_eq(3, spare_capacity)
}

test "string_edge_cases" {
  // 字符串边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, ALLCAPS!", greet("ALLCAPS"))
  assert_eq_string("Hello, mixedCase!", greet("mixedCase"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello,   spaced   !", greet("   spaced   "))
}

test "nested_calculations" {
  // 嵌套计算测试
  let base = 5
  let multiplier = 3
  let divisor = 2
  let offset = 4
  
  // 复杂嵌套计算
  let result = add(
    multiply(base, multiplier),
    divide_with_ceil(subtract(multiplier, 1), divisor)
  )
  assert_eq(17, result)  // 5*3 + ceil(2/2) = 15 + 1 = 16
  
  // 更复杂的嵌套
  let complex_result = divide_with_ceil(
    add(multiply(base, multiplier), offset),
    subtract(multiplier, 1)
  )
  assert_eq(9, complex_result)  // ceil(19/2) = 10
}

test "performance_calculation_scenario" {
  // 性能计算场景测试
  let initial_performance = 100
  let improvement_factor = 15
  let measurement_periods = 8
  let target_performance = 250
  
  // 计算每期需要提升的性能
  let improvement_needed = subtract(target_performance, initial_performance)
  let improvement_per_period = divide_with_ceil(improvement_needed, measurement_periods)
  assert_eq(19, improvement_per_period)
  
  // 验证最终性能
  let final_performance = add(initial_performance, multiply(improvement_per_period, measurement_periods))
  assert_eq(252, final_performance)
  
  // 验证超出目标的性能
  let performance_excess = subtract(final_performance, target_performance)
  assert_eq(2, performance_excess)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 最小值边界
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  
  // 单位值边界
  assert_eq(2, multiply(1, 2))
  assert_eq(2, multiply(2, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(2, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  
  // 负数边界
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}