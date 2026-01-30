// Azimuth 标准单元测试 - 包含8个高质量测试用例
// 使用标准 MoonBit 测试语法编写

test "arithmetc_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-7, 6))
  assert_eq(0, add(100, -100))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-42, multiply(-7, 6))
  assert_eq(0, multiply(7, 0))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-13, subtract(7, 20))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 交换律
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
  
  // 结合律
  assert_eq(
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 158
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配计算
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  let budget_limit = 50000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  
  // 计算需要的付款周期（每周期不超过预算限制）
  let payment_cycles = divide_with_ceil(total_cost, budget_limit)
  
  // 验证结果
  assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  assert_eq(57600, total_cost)  // 768 * 75 = 57600
  assert_eq(2, payment_cycles)  // ceil(57600/50000) = 2
}

test "error_handling_edge_cases" {
  // 错误处理边缘情况测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 245
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(31, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(248, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6200, total_cost)
  
  // 验证预算情况（这里我们预期会超预算，这是一个真实的测试场景）
  assert_true(total_cost > total_budget)
  
  // 计算超出预算的金额
  let budget_overrun = subtract(total_cost, total_budget)
  assert_eq(200, budget_overrun)
}