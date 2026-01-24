// 标准 MoonBit 测试用例 - 为 azimuth 库添加的10个高质量测试
// 包含基本功能、边界条件、数学性质和实际应用场景测试

test "add_function_basic_arithmetic" {
  // 测试加法函数的基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(1, multiply(1, 1))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "divide_with_ceil_packaging_logic" {
  // 测试向上取整除法的包装逻辑
  // 场景：计算装盒问题
  let items = 23
  let box_capacity = 5
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)  // ceil(23/5) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(25, total_capacity)
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(20, add(8, 12))
  assert_eq(96, multiply(12, 8))
  assert_eq(96, multiply(8, 12))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(7, -4))
  assert_eq(0, divide_with_ceil(-1, 4))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：项目资源分配计算
  let team_members = 6
  let hours_per_day = 8
  let working_days = 5
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  assert_eq(240, total_hours)  // 6 * 8 * 5 = 240
  
  // 计算需要的任务周期（每周期40小时）
  let hours_per_cycle = 40
  let cycles_needed = divide_with_ceil(total_hours, hours_per_cycle)
  assert_eq(6, cycles_needed)  // ceil(240/40) = 6
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
  
  // 验证具体计算
  assert_eq(36, multiply(4, 9))    // 4 * (6 + 3) = 36
  assert_eq(24, multiply(4, 6))    // 4 * 6 = 24
  assert_eq(12, multiply(4, 3))    // 4 * 3 = 12
  assert_eq(36, add(24, 12))       // 24 + 12 = 36
}

test "edge_cases_and_boundary_conditions" {
  // 测试边缘情况和边界条件
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_business_scenario" {
  // 测试真实世界业务场景
  // 场景：活动座位安排
  let participants = 85
  let tables = 10
  let seats_per_table = 8
  
  // 计算总座位数
  let total_seats = multiply(tables, seats_per_table)
  assert_eq(80, total_seats)
  
  // 计算额外需要的桌子
  let additional_tables_needed = divide_with_ceil(
    subtract(participants, total_seats),
    seats_per_table
  )
  assert_eq(1, additional_tables_needed)  // ceil((85-80)/8) = 1
  
  // 计算总桌子数
  let total_tables = add(tables, additional_tables_needed)
  assert_eq(11, total_tables)
  
  // 验证最终座位数
  let final_seats = multiply(total_tables, seats_per_table)
  assert_eq(88, final_seats)
}
