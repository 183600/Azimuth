// 新增的标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能

test "add_function_edge_cases" {
  // 加法函数边界情况测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_boundary_values" {
  // 向上取整除法边界值测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "greet_function_special_inputs" {
  // 问候函数特殊输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "subtract_function_various_scenarios" {
  // 减法函数各种场景测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(15, 15))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
  @azimuth.assert_eq(-20, @azimuth.subtract(-10, 10))
  @azimuth.assert_eq(100, @azimuth.subtract(0, -100))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(12, 8))
  @azimuth.assert_eq(96, @azimuth.multiply(12, 8))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  let total_items = 47
  let items_per_package = 10
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(3, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(3, sprints_needed)  // ceil(48/20) = 3
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 95
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(12, tables_needed)
  
  // 计算可用座位数
  let available_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(96, available_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(available_seats, participants)
  @azimuth.assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  @azimuth.assert_true(available_seats >= participants)
}

// 新增10个标准 MoonBit 测试用例
test "standard_add_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(25, @azimuth.add(12, 13))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(7, @azimuth.add(-10, 17))
}

test "standard_multiply_comprehensive" {
  // 乘法函数综合测试
  @azimuth.assert_eq(56, @azimuth.multiply(7, 8))
  @azimuth.assert_eq(0, @azimuth.multiply(15, 0))
  @azimuth.assert_eq(-35, @azimuth.multiply(-5, 7))
  @azimuth.assert_eq(48, @azimuth.multiply(-6, -8))
}

test "standard_divide_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(18, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(16, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 7))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(9, 0))
}

test "standard_greet_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, Standard!", @azimuth.greet("Standard"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 2025!", @azimuth.greet("2025"))
}

test "standard_subtract_comprehensive" {
  // 减法函数综合测试
  @azimuth.assert_eq(9, @azimuth.subtract(15, 6))
  @azimuth.assert_eq(-9, @azimuth.subtract(6, 15))
  @azimuth.assert_eq(0, @azimuth.subtract(11, 11))
  @azimuth.assert_eq(20, @azimuth.subtract(5, -15))
}

test "standard_math_properties_comprehensive" {
  // 数学性质验证测试
  let a = 14
  let b = 9
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(23, @azimuth.add(14, 9))
  @azimuth.assert_eq(126, @azimuth.multiply(14, 9))
}

test "standard_packaging_comprehensive" {
  // 包装计算场景测试
  let total_products = 67
  let products_per_box = 15
  let boxes_needed = @azimuth.divide_with_ceil(total_products, products_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, products_per_box)
  @azimuth.assert_eq(75, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(8, remaining_space)
}

test "standard_workflow_comprehensive" {
  // 工作流计算测试
  // 场景：项目任务分配
  let developers = 7
  let tasks_per_developer = 6
  let total_tasks = @azimuth.multiply(developers, tasks_per_developer)
  
  let sprint_capacity = 10
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(42, total_tasks)  // 7 * 6 = 42
  @azimuth.assert_eq(5, sprints_needed)  // ceil(42/10) = 5
}

test "standard_negative_comprehensive" {
  // 负数处理测试
  @azimuth.assert_eq(-15, @azimuth.add(-8, -7))
  @azimuth.assert_eq(3, @azimuth.subtract(-5, -8))
  @azimuth.assert_eq(40, @azimuth.multiply(-5, -8))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-9, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-9, -5))
}

test "standard_boundary_comprehensive" {
  // 边界值测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(15, 15))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-15, -15))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 15))
  
  // 大数运算
  @azimuth.assert_eq(3000, @azimuth.add(1500, 1500))
  @azimuth.assert_eq(2250000, @azimuth.multiply(1500, 1500))
  
  // 特殊问候情况
  @azimuth.assert_eq_string("Hello,     !", @azimuth.greet("     "))
  @azimuth.assert_eq_string("Hello, symbols#$%!", @azimuth.greet("symbols#$%"))
}