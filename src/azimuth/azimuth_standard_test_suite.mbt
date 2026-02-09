// Azimuth 标准测试套件
// 包含10个高质量的标准 MoonBit 测试用例
// 测试所有核心功能：add, multiply, subtract, greet, divide_with_ceil

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  // 测试加法
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  
  // 测试乘法
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  
  // 测试减法
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边界情况测试
  // 基本向上取整
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 零除数处理
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 零被除数
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "greet_function_internationalization" {
  // 问候函数的国际化测试
  // 基本问候
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 空字符串处理
  assert_eq_string("Hello, !", greet(""))
  
  // Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
}

test "business_packaging_calculation" {
  // 业务包装计算场景测试
  let total_items = 47
  let items_per_package = 12
  
  // 计算需要的包装数量
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(4, packages_needed)  // ceil(47/12) = 4
  
  // 计算总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)  // 4 * 12 = 48
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)  // 48 - 47 = 1
}

test "negative_number_operations" {
  // 负数运算测试
  // 负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  
  // 负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 负数向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_number_handling" {
  // 大数处理测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 大数向上取整除法
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 大数减法
  assert_eq(0, subtract(1000000, 1000000))
  assert_eq(1000000, subtract(1000000, 0))
}

test "complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let buffer_tasks = 3
  let total_tasks_with_buffer = add(total_tasks, buffer_tasks)
  let adjusted_sprints = divide_with_ceil(total_tasks_with_buffer, sprint_capacity)
  
  assert_eq(48, total_tasks)           // 8 * 6 = 48
  assert_eq(4, sprints_needed)        // ceil(48/15) = 4
  assert_eq(51, total_tasks_with_buffer)  // 48 + 3 = 51
  assert_eq(4, adjusted_sprints)      // ceil(51/15) = 4
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
  
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1000))
  assert_eq(0, multiply(1000, 0))
  
  // 边界减法
  assert_eq(0, subtract(0, 0))
  assert_eq(1000, subtract(1000, 0))
  assert_eq(-1000, subtract(0, 1000))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 场景：电商订单处理
  let order_quantity = 23
  let items_per_box = 6
  let cost_per_item = 15
  let shipping_cost_per_box = 8
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_quantity, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(23/6) = 4
  
  // 计算商品总成本
  let item_total_cost = multiply(order_quantity, cost_per_item)
  assert_eq(345, item_total_cost)  // 23 * 15 = 345
  
  // 计算总运费
  let total_shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  assert_eq(32, total_shipping_cost)  // 4 * 8 = 32
  
  // 计算订单总金额
  let order_total = add(item_total_cost, total_shipping_cost)
  assert_eq(377, order_total)  // 345 + 32 = 377
}