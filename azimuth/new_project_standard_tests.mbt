// MoonBit 标准测试用例 - 为 Azimuth 项目新增
// 使用标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(46, add(17, 29))
  assert_eq(493, multiply(17, 29))
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法的小数情况
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 50))
}

test "warehouse_packaging_optimization" {
  // 仓库包装优化场景测试
  let total_products = 156
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(12, remaining_space)
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 9
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6*(9+4) = 6*9 + 6*4 = 78
}

test "event_planning_seating_arrangement" {
  // 活动规划座位安排测试
  let guests = 245
  let tables_available = 30
  let seats_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, seats_per_table)
  assert_eq(31, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, seats_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, guests)
  assert_eq(3, remaining_seats)
}

test "negative_number_arithmetic" {
  // 负数算术运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(0, multiply(-3, 0))
  
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(2, divide_with_ceil(10, -5))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试：项目预算分配
  let team_members = 12
  let hourly_rate = 75
  let hours_per_day = 8
  let project_duration_days = 15
  let budget_per_payment_cycle = 10000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_duration_days))
  assert_eq(1440, total_hours)  // 12 * 8 * 15 = 1440
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(108000, total_budget)  // 1440 * 75 = 108000
  
  // 计算需要的支付周期数
  let payment_cycles = divide_with_ceil(total_budget, budget_per_payment_cycle)
  assert_eq(11, payment_cycles)  // ceil(108000/10000) = 11
}