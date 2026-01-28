// 标准 MoonBit 测试用例 - 为 azimuth 库编写
// 包含10个高质量测试用例，覆盖主要功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(21, @azimuth.multiply(-7, -3))
}

test "subtract_function_basic" {
  // 减法函数基本测试
  @azimuth.assert_eq(4, @azimuth.subtract(7, 3))
  @azimuth.assert_eq(-4, @azimuth.subtract(3, 7))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 2)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 2))
  )
  
  // 零元素性质
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
}

test "boundary_conditions_and_extreme_values" {
  // 边界条件和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))  // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))  // 下溢保护
  
  // 边界值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))  // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))  // 溢出保护
}

test "complex_business_logic_packaging" {
  // 复杂业务逻辑：包装计算测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, box_capacity)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = @azimuth.divide_with_ceil(
    @azimuth.multiply(items, 100), 
    total_capacity
  )
  @azimuth.assert_eq(98, utilization_percent)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数运算
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  
  // 断言函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  let min_budget_per_member = @azimuth.divide_with_ceil(total_budget, team_members)
  
  @azimuth.assert_eq(1667, min_budget_per_member)
  
  // 验证总分配
  let total_allocated = @azimuth.multiply(min_budget_per_member, team_members)
  @azimuth.assert_eq(10002, total_allocated)
  
  // 验证预算超支
  let budget_overhead = @azimuth.subtract(total_allocated, total_budget)
  @azimuth.assert_eq(2, budget_overhead)
  
  // 计算实际每人可用预算（如果有剩余）
  let available_per_person = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(1667, available_per_person)
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let chairs_per_table = 8
  let cost_per_table = 25
  let budget_per_person = 15
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let empty_seats = @azimuth.subtract(total_seats, participants)
  @azimuth.assert_eq(1, empty_seats)
  
  // 计算总桌子成本
  let total_table_cost = @azimuth.multiply(tables_needed, cost_per_table)
  @azimuth.assert_eq(275, total_table_cost)
  
  // 计算总人员预算
  let total_person_budget = @azimuth.multiply(participants, budget_per_person)
  @azimuth.assert_eq(1305, total_person_budget)
  
  // 计算活动总成本
  let total_cost = @azimuth.add(total_table_cost, total_person_budget)
  @azimuth.assert_eq(1580, total_cost)
  
  // 生成活动组织者问候
  let organizer_greeting = @azimuth.greet("活动组织者")
  @azimuth.assert_eq_string("Hello, 活动组织者!", organizer_greeting)
}