// 简单的增强测试覆盖 - 为 azimuth 库添加额外的测试用例

test "add_function_commutative_property" {
  // 测试加法交换律性质
  assert_eq(add(15, 27), add(27, 15))
  assert_eq(add(-8, 13), add(13, -8))
  assert_eq(add(0, 42), add(42, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律性质
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 场景1: 计算会议室需求
  let participants = 23
  let capacity_per_room = 6
  assert_eq(4, divide_with_ceil(participants, capacity_per_room))
  
  // 场景2: 计算运输批次
  let total_items = 85
  let batch_size = 20
  assert_eq(5, divide_with_ceil(total_items, batch_size))
}

test "mathematical_identity_verification" {
  // 测试数学恒等式验证
  let x = 19
  
  // 加法恒等式: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式: x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零性质: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "edge_case_zero_division_handling" {
  // 测试零除法的边界情况处理
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -3))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的综合情况
  // 加法负数测试
  assert_eq(-12, add(-5, -7))
  assert_eq(3, add(-8, 11))
  assert_eq(-8, add(3, -11))
  
  // 乘法负数测试
  assert_eq(35, multiply(-5, -7))
  assert_eq(-40, multiply(-5, 8))
  assert_eq(-24, multiply(6, -4))
  
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(-1, divide_with_ceil(5, -8))
}

test "real_world_business_calculation" {
  // 真实世界业务计算测试
  // 场景: 计算项目资源分配
  let project_duration_days = 45
  let team_members = 8
  let working_hours_per_day = 8
  
  // 计算总工时
  let total_hours = multiply(project_duration_days, multiply(team_members, working_hours_per_day))
  assert_eq(2880, total_hours)  // 45 * 8 * 8 = 2880
  
  // 计算需要的冲刺次数（每个冲刺80小时）
  let sprint_hours = 80
  let sprints_needed = divide_with_ceil(total_hours, sprint_hours)
  assert_eq(36, sprints_needed)  // ceil(2880/80) = 36
}