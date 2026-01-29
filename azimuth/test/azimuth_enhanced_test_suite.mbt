// Azimuth 增强测试套件
// 包含10个标准MoonBit测试用例，覆盖各种功能和场景

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(42, @azimuth.multiply(-6, -7))
  
  @azimuth.assert_eq(5, @azimuth.subtract(12, 7))
  @azimuth.assert_eq(-5, @azimuth.subtract(7, 12))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, empty!", @azimuth.greet(""))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 5), @azimuth.add(x, @azimuth.add(y, 5)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), 2), @azimuth.multiply(x, @azimuth.multiply(y, 2)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(11, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, working_days))
  @azimuth.assert_eq(960, total_hours)
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(total_hours, tasks_per_hour)
  @azimuth.assert_eq(1920, total_tasks)
  
  // 计算需要的冲刺次数（每冲刺250个任务）
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, 250)
  @azimuth.assert_eq(8, sprints_needed)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  
  // 单位值测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  
  // 大数测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
}

test "string_processing_validation" {
  // 字符串处理验证测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
  @azimuth.assert_eq_string("Hello, mixedCASE!", @azimuth.greet("mixedCASE"))
  @azimuth.assert_eq_string("Hello,   spaces   !", @azimuth.greet("   spaces   "))
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  
  // 极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动组织
  let participants = 87
  let chairs_per_table = 10
  let cost_per_chair = 25
  let budget = 2500
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  // 计算剩余座位
  let remaining_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, remaining_chairs)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  @azimuth.assert_eq(2250, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.subtract(budget, total_cost)
  @azimuth.assert_eq(250, remaining_budget)
}