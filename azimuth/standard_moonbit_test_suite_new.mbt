// 标准 MoonBit 测试套件 - 新增10个高质量测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  assert_eq(100, multiply(10, 10))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(0, multiply(0, 999))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 25
  let y = 12
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算仓库存储需求
  let total_products = 157
  let pallet_capacity = 24
  let pallets_needed = divide_with_ceil(total_products, pallet_capacity)
  
  assert_eq(7, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_workflow_pipeline" {
  // 复杂工作流流水线测试
  // 场景：多阶段数据处理
  let input_data = 50
  let stage1_multiplier = 3
  let stage2_divisor = 4
  let stage3_addition = 7
  
  // 处理流水线
  let stage1_result = multiply(input_data, stage1_multiplier)
  let stage2_result = divide_with_ceil(stage1_result, stage2_divisor)
  let final_result = add(stage2_result, stage3_addition)
  
  // 验证结果：((50 * 3) / 4) + 7 = ceil(150/4) + 7 = 38 + 7 = 45
  assert_eq(45, final_result)
}

test "error_handling_boundary_conditions" {
  // 错误处理边界条件测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  
  // 极值边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 25000
  let departments = 4
  
  // 使用向上取整确保每个部门都有足够预算
  let budget_per_department = divide_with_ceil(total_budget, departments)
  assert_eq(6250, budget_per_department)
  
  // 验证总分配
  let total_allocated = multiply(budget_per_department, departments)
  assert_eq(25000, total_allocated)
  
  // 验证余量
  let surplus = subtract(total_allocated, total_budget)
  assert_eq(0, surplus)
}

test "boolean_assertions_comprehensive" {
  // 布尔断言综合测试
  // 测试布尔条件断言
  assert_true(5 > 3)
  assert_true(10 == 10)
  assert_true(-5 < 0)
  
  assert_false(5 < 3)
  assert_false(10 != 10)
  assert_false(-5 > 0)
  
  // 复杂布尔表达式
  assert_true(add(5, 3) == multiply(2, 4))
  assert_false(multiply(3, 4) == add(10, 5))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议活动安排
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available + 1)  // 允许增加一张桌子
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 计算预算差额
  let budget_difference = subtract(total_cost, total_budget)
  assert_eq(200, budget_difference)
  
  // 验证座位充足
  assert_true(total_seats >= attendees)
}