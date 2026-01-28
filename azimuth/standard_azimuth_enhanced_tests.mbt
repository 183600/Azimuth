// 标准 Azimuth 增强测试用例
// 包含 8 个高质量的 MoonBit 测试用例，覆盖主要功能

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(3, -5))
  assert_eq(0, add(-10, 10))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-21, multiply(7, -3))
  assert_eq(0, multiply(7, 0))
  
  assert_eq(3, divide_with_ceil(25, 9))
  assert_eq(-2, divide_with_ceil(-15, 9))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 3)),
    add(multiply(a, b), multiply(a, 3))
  )
  
  // 结合律验证
  assert_eq(
    add(add(a, b), 5),
    add(a, add(b, 5))
  )
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_items = 158
  let box_capacity = 25
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(17, remaining_space)
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // �数边界测试
  assert_eq(-15, add(-10, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 4))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_size = 6
  let work_days = 15
  let daily_hours = 8
  let hour_rate = 50
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(work_days, daily_hours))
  assert_eq(720, total_hours)
  
  // 计算总预算
  let total_budget = multiply(total_hours, hour_rate)
  assert_eq(36000, total_budget)
  
  // 计算支付周期（每周期不超过10000）
  let payment_cycles = divide_with_ceil(total_budget, 10000)
  assert_eq(4, payment_cycles)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 极端边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景：活动座位安排
  let participants = 95
  let tables = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(12, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(96, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, participants)
  assert_eq(1, empty_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
  
  // 场景：运输成本计算
  let packages = 47
  let truck_capacity = 10
  let cost_per_truck = 150
  
  let trucks_needed = divide_with_ceil(packages, truck_capacity)
  let total_cost = multiply(trucks_needed, cost_per_truck)
  
  assert_eq(5, trucks_needed)
  assert_eq(750, total_cost)
}