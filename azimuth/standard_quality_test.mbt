// 标准 MoonBit 测试用例 - 高质量测试覆盖
// 测试 azimuth 库的核心功能

test "add_function_comprehensive_test" {
  // 加法函数综合测试：正数、负数、零值
  azimuth.assert_eq(15, azimuth.add(8, 7))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-15, azimuth.add(-8, -7))
  azimuth.assert_eq(1, azimuth.add(-8, 9))
  azimuth.assert_eq(100000, azimuth.add(50000, 50000))
}

test "multiply_function_boundary_test" {
  // 乘法函数边界测试：零值、单位元、负数
  azimuth.assert_eq(0, azimuth.multiply(12345, 0))
  azimuth.assert_eq(12345, azimuth.multiply(12345, 1))
  azimuth.assert_eq(-12345, azimuth.multiply(12345, -1))
  azimuth.assert_eq(12345, azimuth.multiply(-12345, -1))
  azimuth.assert_eq(1000000, azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试：各种除数情况
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 100))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(100, 0))
}

test "greet_function_unicode_test" {
  // 问候函数Unicode测试：多语言和特殊字符
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  azimuth.assert_eq_string("Hello, café!", azimuth.greet("café"))
  azimuth.assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  azimuth.assert_eq_string("Hello, empty!", azimuth.greet(""))
}

test "mathematical_properties_test" {
  // 数学性质验证测试：交换律、分配律
  let x = 12
  let y = 8
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
  
  // 分配律验证
  azimuth.assert_eq(
    azimuth.multiply(x, azimuth.add(y, 3)),
    azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, 3))
  )
}

test "business_logic_packaging_test" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = azimuth.divide_with_ceil(total_items, box_capacity)
  
  azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(1, remaining_space)
}

test "complex_workflow_test" {
  // 复杂工作流测试：项目管理场景
  let team_size = 6
  let tasks_per_person = 5
  let sprint_capacity = 15
  
  // 计算总任务数
  let total_tasks = azimuth.multiply(team_size, tasks_per_person)
  azimuth.assert_eq(30, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  azimuth.assert_eq(2, sprints_needed)
  
  // 计算实际处理能力
  let actual_capacity = azimuth.multiply(sprints_needed, sprint_capacity)
  azimuth.assert_eq(30, actual_capacity)
}

test "error_handling_robustness_test" {
  // 错误处理健壮性测试：边界条件和异常情况
  // 除零错误的各种情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(100, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  
  // 极值测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
}

test "resource_allocation_optimization_test" {
  // 资源分配优化测试：预算分配场景
  let total_budget = 10000
  let departments = 3
  
  // 计算每个部门的最大分配
  let max_allocation = azimuth.divide_with_ceil(total_budget, departments)
  azimuth.assert_eq(3334, max_allocation)
  
  // 验证总分配
  let total_allocated = azimuth.multiply(max_allocation, departments)
  azimuth.assert_eq(10002, total_allocated)
  
  // 计算超出预算的金额
  let over_budget = azimuth.subtract(total_allocated, total_budget)
  azimuth.assert_eq(2, over_budget)
}

test "real_world_event_planning_test" {
  // 真实世界事件规划测试：活动安排
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = azimuth.divide_with_ceil(participants, chairs_per_table)
  azimuth.assert_eq(11, tables_needed)
  
  // 验证桌子是否足够
  azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = azimuth.multiply(tables_needed, chairs_per_table)
  azimuth.assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth.subtract(total_seats, participants)
  azimuth.assert_eq(1, remaining_seats)
}