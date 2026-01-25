// 高质量标准 MoonBit 测试用例
// 为 azimuth 库添加的 10 个标准测试用例
// 覆盖基本功能、边界情况和实际应用场景

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(1, azimuth.add(1, 0))
  assert_eq(-1, azimuth.add(-1, 0))
  assert_eq(2147483647, azimuth.add(2147483646, 1))
  assert_eq(-2147483648, azimuth.add(-2147483647, -1))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, azimuth.multiply(0, 12345))
  assert_eq(12345, azimuth.multiply(1, 12345))
  assert_eq(-12345, azimuth.multiply(-1, 12345))
  assert_eq(12345, azimuth.multiply(-1, -12345))
  assert_eq(1000000, azimuth.multiply(1000, 1000))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  assert_eq_string("Hello, café!", azimuth.greet("café"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, !", azimuth.greet(""))
}

test "divide_with_ceil_logistics_calculation" {
  // 测试向上取整除法的物流计算场景
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = azimuth.divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = azimuth.multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, packages)
  assert_eq(3, unused_space)
}

test "complex_project_management" {
  // 测试复杂的项目管理计算
  let team_members = 6
  let tasks_per_person = 8
  let total_tasks = azimuth.multiply(team_members, tasks_per_person)
  assert_eq(48, total_tasks)
  
  let sprint_duration = 10
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, sprint_duration)
  assert_eq(5, sprints_needed)
  
  let tasks_per_sprint = azimuth.divide_with_ceil(total_tasks, sprints_needed)
  assert_eq(10, tasks_per_sprint)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
  
  // 分配律
  assert_eq(
    azimuth.multiply(x, azimuth.add(y, 3)),
    azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, 3))
  )
  
  // 加法恒等式
  assert_eq(x, azimuth.add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, azimuth.multiply(x, 1))
}

test "financial_budget_allocation" {
  // 测试财务预算分配
  let total_budget = 10000
  let departments = 3
  
  // 平均分配（向上取整确保预算充足）
  let budget_per_dept = azimuth.divide_with_ceil(total_budget, departments)
  assert_eq(3334, budget_per_dept)
  
  // 验证总分配
  let total_allocated = azimuth.multiply(budget_per_dept, departments)
  assert_eq(10002, total_allocated)
  
  // 计算额外需要的预算
  let extra_budget = subtract(total_allocated, total_budget)
  assert_eq(2, extra_budget)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 除零情况
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  assert_eq(0, azimuth.divide_with_ceil(0, 1))
  assert_eq(0, azimuth.divide_with_ceil(0, -1))
}

test "string_processing_special_cases" {
  // 测试字符串处理的特殊情况
  assert_eq_string("Hello, single!", azimuth.greet("single"))
  assert_eq_string("Hello, space test!", azimuth.greet("space test"))
  assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  assert_eq_string("Hello, !@#$%^!", azimuth.greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", azimuth.greet("mixedCASE"))
  assert_eq_string("Hello,   !", azimuth.greet("  "))
}

test "resource_optimization_problem" {
  // 测试资源优化问题
  // 场景：仓库存储优化
  let item1_quantity = 25
  let item1_capacity = 8
  let boxes_1 = azimuth.divide_with_ceil(item1_quantity, item1_capacity)
  
  let item2_quantity = 47
  let item2_capacity = 12
  let boxes_2 = azimuth.divide_with_ceil(item2_quantity, item2_capacity)
  
  let total_boxes = azimuth.add(boxes_1, boxes_2)
  assert_eq(8, total_boxes)
  
  // 计算总存储容量
  let capacity_1 = azimuth.multiply(boxes_1, item1_capacity)
  let capacity_2 = azimuth.multiply(boxes_2, item2_capacity)
  let total_capacity = azimuth.add(capacity_1, capacity_2)
  assert_eq(80, total_capacity)
  
  // 计算利用率
  let total_items = azimuth.add(item1_quantity, item2_quantity)
  let utilization = azimuth.multiply(azimuth.divide_with_ceil(total_items, total_capacity), 100)
  assert_eq(90, utilization)  // 72/80 = 0.9 = 90%
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}