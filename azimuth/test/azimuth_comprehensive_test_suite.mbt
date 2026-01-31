// Azimuth 综合测试套件
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.subtract(10, 8))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 4)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 4))
  )
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(12, 8))
  @azimuth.assert_eq(96, @azimuth.multiply(12, 8))
}

test "boundary_conditions_and_error_handling" {
  // 边界条件和错误处理测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, éspanol!", @azimuth.greet("éspanol"))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：计算项目完成时间和成本
  let tasks = 25
  let team_members = 4
  let tasks_per_person_per_day = 2
  let daily_cost_per_person = 100
  
  // 计算每日处理能力
  let daily_capacity = @azimuth.multiply(team_members, tasks_per_person_per_day)
  @azimuth.assert_eq(8, daily_capacity)
  
  // 计算需要的天数
  let days_needed = @azimuth.divide_with_ceil(tasks, daily_capacity)
  @azimuth.assert_eq(4, days_needed)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(
    @azimuth.multiply(days_needed, team_members),
    daily_cost_per_person
  )
  @azimuth.assert_eq(1600, total_cost)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let departments = 3
  
  // 计算每个部门的平均预算
  let avg_budget = @azimuth.divide_with_ceil(total_budget, departments)
  @azimuth.assert_eq(3334, avg_budget)
  
  // 验证总分配预算
  let total_allocated = @azimuth.multiply(avg_budget, departments)
  @azimuth.assert_eq(10002, total_allocated)
  
  // 计算预算溢出
  let budget_overflow = @azimuth.subtract(total_allocated, total_budget)
  @azimuth.assert_eq(2, budget_overflow)
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  let neg_a = -15
  let pos_b = 8
  let neg_c = -3
  
  // 负数加法
  @azimuth.assert_eq(-7, @azimuth.add(neg_a, pos_b))
  @azimuth.assert_eq(-18, @azimuth.add(neg_a, neg_c))
  @azimuth.assert_eq(5, @azimuth.add(pos_b, neg_c))
  
  // 负数乘法
  @azimuth.assert_eq(-120, @azimuth.multiply(neg_a, pos_b))
  @azimuth.assert_eq(45, @azimuth.multiply(neg_a, neg_c))
  @azimuth.assert_eq(-24, @azimuth.multiply(pos_b, neg_c))
  
  // 负数除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(neg_a, pos_b))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(neg_a, neg_c))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(pos_b, neg_c))
}

test "large_numbers_operations" {
  // 大数运算测试
  let large_a = 10000
  let large_b = 25000
  
  // 大数加法
  @azimuth.assert_eq(35000, @azimuth.add(large_a, large_b))
  
  // 大数乘法
  @azimuth.assert_eq(250000000, @azimuth.multiply(large_a, large_b))
  
  // 大数除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_a, large_b))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(large_b, large_a))
  
  // 极值测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let tables_available = 10
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, participants)
  @azimuth.assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_seats, cost_per_chair)
  @azimuth.assert_eq(2200, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(total_budget, total_cost)
  @azimuth.assert_eq(800, remaining_budget)
}