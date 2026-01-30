// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加10个高质量测试用例

test "mathematical_commutative_law" {
  // 测试数学交换律
  let a = 42
  let b = 17
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_precision_handling" {
  // 测试向上取整除法的精度处理
  // 测试各种余数情况
  assert_eq(2, divide_with_ceil(3, 2))      // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))      // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))      // 3.5 -> 4
  assert_eq(2, divide_with_ceil(4, 3))      // 1.33 -> 2
  assert_eq(1, divide_with_ceil(2, 3))      // 0.67 -> 1
}

test "business_inventory_calculation" {
  // 业务场景：库存计算测试
  let total_products = 157
  let boxes_per_pallet = 24
  let pallets_needed = divide_with_ceil(total_products, boxes_per_pallet)
  
  // 验证需要的托盘数量
  assert_eq(7, pallets_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
}

test "string_processing_unicode_support" {
  // 字符串处理和Unicode支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   spaces!", greet("  spaces"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  // 计算需要的冲刺数（每冲刺最多100个任务）
  let sprint_capacity = 100
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证结果
  assert_eq(960, total_hours)     // 8 * 8 * 15 = 960
  assert_eq(1920, total_tasks)    // 960 * 2 = 1920
  assert_eq(20, sprints_needed)   // ceil(1920/100) = 20
}

test "performance_large_scale_computation" {
  // 大规模计算性能测试
  let large_number = 100000
  let iterations = 10
  
  // 测试大数运算
  assert_eq(200000, add(large_number, large_number))
  assert_eq(10000000000, multiply(large_number, large_number))
  assert_eq(1, divide_with_ceil(large_number, large_number))
  assert_eq(100001, divide_with_ceil(large_number, large_number - 1))
  
  // 测试连续运算的性能
  let result = large_number
  for i = 0; i < iterations; i = i + 1 {
    result = add(result, 1)
  }
  assert_eq(large_number + iterations, result)
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 测试零元素性质
  assert_eq(5, add(5, 0))      // 加法单位元
  assert_eq(0, multiply(5, 0))  // 乘法零元
  assert_eq(5, multiply(5, 1))  // 乘法单位元
  assert_eq(0, subtract(5, 5))  // 自减为零
  
  // 测试负数性质
  assert_eq(0, add(5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(10, subtract(5, -5))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 250
  let tables_available = 25
  let chairs_per_table = 10
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(25, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(250, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(0, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3750, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(250, remaining_budget)
}